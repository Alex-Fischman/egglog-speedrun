//! This module defines the representation of a `function` in `egglog`.

use crate::*;

/// A single function in an `egglog` program.
pub struct Table {
    /// The name of this table in the database.
    name: String,
    /// The schema for the columns of the `egglog` function.
    schema: Vec<Type>,
    /// The merge function for this table. `None` means "default".
    merge: Option<Expr>,
    /// The data in this table, stored row-wise and indexed by `RowId`s.
    data: Vec<Value>,
    /// The rows in this table indexed by all of the input columns.
    function: HashMap<Vec<Value>, RowId>,
    /// The rows in this table indexed by the values in each column.
    columns: Vec<HashMap<Value, HashSet<RowId>>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct RowId(usize);

/// Returns the data in the given row.
fn get_row<'a>(data: &'a [Value], schema: &[Type], id: RowId) -> &'a [Value] {
    &data[id.0 * schema.len()..(id.0 + 1) * schema.len()]
}

impl Table {
    /// Create a new `Table` with the given schema.
    #[must_use]
    pub fn new(name: String, schema: Vec<Type>, merge: Option<Expr>) -> Table {
        Table {
            columns: vec![HashMap::new(); schema.len()],
            name,
            schema,
            merge,
            data: Vec::new(),
            function: HashMap::new(),
        }
    }

    /// Returns a `RowId` with the total number of rows in the table (ignoring liveness).
    fn length_id(&self) -> RowId {
        RowId(self.data.len() / self.schema.len())
    }

    /// Removes a row from all indices so it can't be referenced except by `RowId`.
    fn remove_row(&mut self, id: RowId) {
        let row = get_row(&self.data, &self.schema, id);
        self.function.remove(&row[..row.len() - 1]);
        for (column, x) in self.columns.iter_mut().zip(row) {
            column.get_mut(x).unwrap().remove(&id);
        }
    }

    /// Add a row to the table, merging if a row with the given inputs already exists.
    /// Returns true if the either the table or `sorts` was changed.
    pub fn insert(&mut self, mut row: Vec<Value>, sorts: &mut Sorts) -> Result<bool, String> {
        if row.len() != self.schema.len() {
            return Err(format!(
                "expected row of length {}, found row of length {} for {}",
                self.schema.len(),
                row.len(),
                self.name,
            ));
        }
        for (t, x) in self.schema.iter().zip(&row) {
            x.assert_type(t)?;
        }

        // If there's a conflict, remove the old row
        if let Some(&id) = self.function.get(&row[..row.len() - 1]) {
            let old = get_row(&self.data, &self.schema, id).last().unwrap();
            let new = row.last_mut().unwrap();
            let mut changed = false;
            *new = match &self.merge {
                Some(expr) => expr.evaluate_mut(
                    &HashMap::from([("old", old.clone()), ("new", new.clone())]),
                    &mut HashMap::new(),
                    sorts,
                )?,
                None if old == new => new.clone(),
                None => match (old.clone(), new.clone(), self.schema.last().unwrap()) {
                    (Value::Sort(old), Value::Sort(new), Type::Sort(s)) => {
                        changed |= sorts.get_mut(s).unwrap().union(old, new)?;
                        Value::Sort(sorts.get_mut(s).unwrap().find(old))
                    }
                    _ => return Err(format!("{old} != {new} in {}", self.name)),
                },
            };
            if old == new && !changed {
                return Ok(false);
            }
            self.remove_row(id);
        }

        // Append the new row
        let id = self.length_id();
        self.function.insert(row[..row.len() - 1].to_vec(), id);
        for (column, x) in self.columns.iter_mut().zip(&row) {
            column.entry(x.clone()).or_default().insert(id);
        }
        self.data.append(&mut row);
        Ok(true)
    }

    /// Get the output value of a row for some inputs. If there isn't a value, but the
    /// output type is a sort, create a new sort element, add it to the table, and return it.
    pub fn get(&mut self, mut xs: Vec<Value>, sorts: &mut Sorts) -> Result<Option<Value>, String> {
        Ok(if let Some(&id) = self.function.get(&xs) {
            Some(
                get_row(&self.data, &self.schema, id)
                    .last()
                    .unwrap()
                    .clone(),
            )
        } else if let Type::Sort(sort) = &self.schema.last().unwrap() {
            let y = Value::Sort(sorts.get_mut(sort).unwrap().new_key(()));
            xs.push(y.clone());
            self.insert(xs, sorts)?;
            Some(y)
        } else {
            None
        })
    }

    /// Rebuild the indices so that each `Value::Sort` holds a canonical key.
    /// Does not run to fixpoint, even if rebuilding changes `sorts`.
    pub fn rebuild(&mut self, sorts: &mut Sorts) -> Result<bool, String> {
        let mut changed = false;
        // For each row, replace it with its canonicalized version.
        let ids: Vec<_> = self.function.values().copied().collect();
        for id in ids {
            let row = get_row(&self.data, &self.schema, id)
                .iter()
                .zip(&self.schema)
                .map(|(v, t)| match (v, t) {
                    (Value::Sort(v), Type::Sort(s)) => {
                        Value::Sort(sorts.get_mut(s).unwrap().find(*v))
                    }
                    _ => v.clone(),
                })
                .collect();
            if row != get_row(&self.data, &self.schema, id) {
                changed = true;
                self.remove_row(id);
                self.insert(row, sorts)?;
            }
        }
        Ok(changed)
    }

    /// Convert an iterator of ids to an iterator of rows.
    fn ids_to_rows<'a>(
        &'a self,
        ids: impl Iterator<Item = &'a RowId>,
    ) -> impl Iterator<Item = &'a [Value]> {
        ids.map(|&id| get_row(&self.data, &self.schema, id))
    }

    /// Get all of the live rows in this table.
    pub fn rows(&self) -> impl Iterator<Item = &[Value]> {
        self.ids_to_rows(self.function.values())
    }

    /// Get the (at most) one row in this table with the specific inputs in the input columns.
    /// This method never changes `self`; notably, it will not create new sort elements.
    pub fn rows_with_inputs(&self, xs: &[Value]) -> impl Iterator<Item = &[Value]> {
        self.ids_to_rows(self.function.get(xs).into_iter())
    }

    /// Get all of the rows that have a specific value in a specific column.
    pub fn rows_with_value_in_column(
        &self,
        value: &Value,
        column: usize,
    ) -> impl Iterator<Item = &[Value]> {
        self.ids_to_rows(self.columns[column].get(value).into_iter().flatten())
    }

    /// Get the number of live rows in the table.
    #[must_use]
    pub fn num_rows(&self) -> usize {
        self.function.len()
    }

    /// Get the length of a column index for a specific value.
    #[must_use]
    pub fn num_rows_with_value_in_column(&self, value: &Value, column: usize) -> usize {
        self.columns[column].get(value).map_or(0, HashSet::len)
    }

    /// Get the number of possible values in a specific column.
    #[must_use]
    pub fn num_values_in_column(&self, column: usize) -> usize {
        self.columns[column].len()
    }

    /// Get all of the values in a column.
    pub fn values_in_column(&self, column: usize) -> impl Iterator<Item = &Value> {
        self.columns[column].keys()
    }

    /// Check if a specific value ever appears in a column.
    #[must_use]
    pub fn is_value_in_column(&self, value: &Value, column: usize) -> bool {
        self.columns[column].contains_key(value)
    }
}
