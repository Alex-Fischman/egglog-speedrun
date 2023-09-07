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
    /// The indices into the data for this table. None means don't care.
    indices: HashMap<Vec<Option<Value>>, BTreeSet<RowId>>,
    /// A default set to return a reference to.
    empty: BTreeSet<RowId>,
    /// The rows that were added in the last iteration.
    prev: Range<RowId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
            name,
            schema,
            merge,
            data: Vec::new(),
            indices: HashMap::new(),
            empty: BTreeSet::new(),
            prev: RowId(0)..RowId(0),
        }
    }

    /// Get the number of columns in this table.
    #[must_use]
    pub fn width(&self) -> usize {
        self.schema.len()
    }

    /// Returns a `RowId` with the total number of rows in the table (ignoring liveness).
    fn length_id(&self) -> RowId {
        RowId(self.data.len() / self.schema.len())
    }

    /// Get all the `RowId`s corresponding to the given values.
    fn get_ids(&self, row: &[Option<Value>]) -> &BTreeSet<RowId> {
        self.indices.get(row).unwrap_or(&self.empty)
    }

    /// Get the `RowId` corresponding to the given inputs.
    fn get_id(&self, xs: &[Value]) -> Option<RowId> {
        let row: Vec<_> = xs.iter().cloned().map(Option::Some).chain([None]).collect();
        let ids = self.get_ids(&row);
        match ids.len() {
            0 => None,
            1 => Some(*ids.iter().next().unwrap()),
            _ => unreachable!(),
        }
    }

    /// Removes a row from all indices so it can't be referenced except by `RowId`.
    fn remove_row(&mut self, id: RowId) {
        for set in self.indices.values_mut() {
            set.remove(&id);
        }
    }

    /// This function advances the iteration pointers for semi-naive.
    pub fn iteration_start(&mut self) {
        self.prev = self.prev.end..self.length_id();
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
        if let Some(id) = self.get_id(&row[..row.len() - 1]) {
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
        for i in 0..2_usize.pow(u32::try_from(row.len()).unwrap()) {
            let row = row.iter().enumerate().map(|(j, v)| match (i >> j) & 1 {
                0 => None,
                1 => Some(v.clone()),
                _ => unreachable!(),
            });
            self.indices.entry(row.collect()).or_default().insert(id);
        }
        self.data.append(&mut row);
        Ok(true)
    }

    /// Get the output value of a row for some inputs. If there isn't a value, but the
    /// output type is a sort, create a new sort element, add it to the table, and return it.
    pub fn get(&mut self, mut xs: Vec<Value>, sorts: &mut Sorts) -> Result<Option<Value>, String> {
        Ok(if let Some(id) = self.get_id(&xs) {
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
        let ids = self.get_ids(&vec![None; self.schema.len()]).clone();
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

    /// Get the output in this table with the specific inputs in the input columns.
    /// This method never changes `self`; notably, it will not create new sort elements.
    #[must_use]
    pub fn evaluate(&self, xs: &[Value]) -> Option<Value> {
        assert_eq!(self.schema.len() - 1, xs.len());
        self.get_id(xs).map(|id| {
            get_row(&self.data, &self.schema, id)
                .last()
                .unwrap()
                .clone()
        })
    }

    /// Get the set of rows with specific values in specific columns. `None` means "don't care".
    pub fn rows(&self, row: &[Option<Value>], i: Iteration) -> impl Iterator<Item = &[Value]> {
        assert_eq!(self.schema.len(), row.len());
        self.get_ids(row)
            .range(match i {
                Iteration::Past => RowId(0)..self.prev.start,
                Iteration::Prev => self.prev.clone(),
                Iteration::All => RowId(0)..self.length_id(),
            })
            .map(|&id| get_row(&self.data, &self.schema, id))
    }
}

/// Which rows of the table to get when doing seminaive
#[derive(Clone, Copy)]
pub enum Iteration {
    /// Only get rows from before the last iteration.
    Past,
    /// Only get rows that were added in the last iteration.
    Prev,
    /// Get all rows.
    All,
}
