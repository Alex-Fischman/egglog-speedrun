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
    /// The data in this table indexed by `RowId`s.
    primary: Vec<(Vec<Value>, bool)>,
    /// The rows in this table indexed by all of the input columns.
    function: HashMap<Vec<Value>, RowId>,
    /// The rows in this table indexed by the values in each column.
    columns: Vec<HashMap<Value, HashSet<RowId>>>,
}

type RowId = usize;

impl Table {
    /// Create a new `Table` with the given schema.
    #[must_use]
    pub fn new(name: String, schema: Vec<Type>, merge: Option<Expr>) -> Table {
        Table {
            columns: vec![HashMap::new(); schema.len()],
            name,
            schema,
            merge,
            primary: Vec::new(),
            function: HashMap::new(),
        }
    }

    /// Get the number of live rows in the table.
    #[must_use]
    pub fn len(&self) -> usize {
        self.function.len()
    }

    /// Check if this table is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.function.is_empty()
    }

    /// Get the schema for this table.
    #[must_use]
    pub fn schema(&self) -> &[Type] {
        &self.schema
    }

    /// Adds a row to the table, without checking if the inputs are already present.
    fn append_row(&mut self, row: Vec<Value>) {
        let id = self.primary.len();
        self.function.insert(row[..row.len() - 1].to_vec(), id);
        for (column, &x) in self.columns.iter_mut().zip(&row) {
            column.entry(x).or_default().insert(id);
        }
        self.primary.push((row, true));
    }

    /// Removes a row from the table by marking it as dead.
    fn remove_row(&mut self, id: RowId) {
        let (row, live) = &mut self.primary[id];
        *live = false;
        self.function.remove(&row[..row.len() - 1]);
        for (column, x) in self.columns.iter_mut().zip(row) {
            column.get_mut(x).unwrap().remove(&id);
        }
    }

    /// Add a row to the table, merging if a row with the given inputs already exists.
    /// Returns true if the table was changed.
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

        if let Some(&id) = self.function.get(&row[..row.len() - 1]) {
            let old = &self.primary[id].0[self.schema.len() - 1];
            let new = &mut row[self.schema.len() - 1];
            *new = match &self.merge {
                Some(expr) => expr.evaluate_mut(
                    &HashMap::from([("old", *old), ("new", *new)]),
                    &mut HashMap::new(),
                    sorts,
                )?,
                None if old == new => *new,
                None => return Err(format!("{old} != {new} in {}", self.name)),
            };
            if old == new {
                return Ok(false);
            }
            self.remove_row(id);
        }

        self.append_row(row);
        Ok(true)
    }

    /// Get the output value of a row for some inputs. If there isn't a value, but the
    /// output type is a sort, create a new sort element, add it to the table, and return it.
    pub fn get(&mut self, mut xs: Vec<Value>, sorts: &mut Sorts) -> Result<Option<Value>, String> {
        Ok(if let Some(&id) = self.function.get(&xs) {
            Some(self.primary[id].0[self.schema.len() - 1])
        } else if let Type::Sort(sort) = &self.schema[self.schema.len() - 1] {
            let y = Value::Sort(sorts.get_mut(sort).unwrap().new_key(()));
            xs.push(y);
            self.insert(xs, sorts)?;
            Some(y)
        } else {
            None
        })
    }

    /// Rebuild the indices so that each `Value::Sort` holds a canonical key.
    pub fn rebuild(&mut self, sorts: &mut Sorts) -> Result<bool, String> {
        let mut ever_changed = false;
        let mut changed = true;
        while changed {
            changed = false;
            let ids: Vec<_> = self.function.values().copied().collect();
            for id_a in ids {
                for (column, t) in self.schema.clone().into_iter().enumerate() {
                    if let Type::Sort(s) = t {
                        let uf = sorts.get_mut(&s).unwrap();
                        let mut row = self.primary[id_a].0.clone();
                        let Value::Sort(old) = row[column] else {
                            unreachable!("we check types on insertion")
                        };
                        let new = uf.find(old);
                        if old != new {
                            // Remove the bad row.
                            changed = true;
                            self.remove_row(id_a);

                            // Either do congruence closure or re-append the updated row.
                            row[column] = Value::Sort(new);
                            if let Some(&id_b) = self.function.get(&row[..row.len() - 1]) {
                                let (Value::Sort(a), Value::Sort(b)) =
                                    (row[row.len() - 1], self.primary[id_b].0[row.len() - 1])
                                else {
                                    unreachable!("we check types on insertion")
                                };
                                uf.union(a, b)?;
                            } else {
                                self.append_row(row);
                            }
                        }
                    }
                }
            }
            ever_changed |= changed;
        }
        Ok(ever_changed)
    }

    /// Get all of the live rows in this table.
    pub fn rows(&self) -> impl Iterator<Item = &[Value]> {
        self.function
            .values()
            .map(|&id| self.primary[id].0.as_slice())
    }

    /// Get the (at most) one row in this table with the specific inputs in the input columns.
    /// This method never changes `self`; notably, it will not create new sort elements.
    pub fn rows_with_inputs(&self, xs: &[Value]) -> impl Iterator<Item = &[Value]> {
        self.function
            .get(xs)
            .into_iter()
            .map(|&id| self.primary[id].0.as_slice())
    }

    /// Get all of the rows that have a specific value in a specific column.
    pub fn rows_with_value_in_column(
        &self,
        value: Value,
        column: usize,
    ) -> impl Iterator<Item = &[Value]> {
        self.columns[column]
            .get(&value)
            .into_iter()
            .flat_map(|set| set.iter().map(|&id| self.primary[id].0.as_slice()))
    }
}
