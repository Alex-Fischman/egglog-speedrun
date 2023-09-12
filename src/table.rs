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

    /// Get the number of rows in this table.
    fn height(&self) -> usize {
        self.data.len() / self.width()
    }

    /// Get the number of live rows in this table.
    #[must_use]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.get_ids(&vec![None; self.width()]).len()
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
        let row = get_row(&self.data, &self.schema, id);
        for i in 0..2_usize.pow(u32::try_from(row.len()).unwrap()) {
            let row = row.iter().enumerate().map(|(j, v)| match (i >> j) & 1 {
                0 => None,
                1 => Some(v.clone()),
                _ => unreachable!(),
            });
            if let Some(set) = self.indices.get_mut(&row.collect::<Vec<_>>()) {
                set.remove(&id);
            }
        }
    }

    /// This function advances the iteration pointers for semi-naive.
    pub fn iteration_start(&mut self) {
        self.prev = self.prev.end..RowId(self.height());
    }

    /// Add a row to the table, merging if a row with the given inputs already exists.
    /// Returns true if the either the table or `sorts` was changed.
    pub fn insert(&mut self, mut row: Vec<Value>, sorts: &mut Sorts) -> Result<bool, String> {
        let types: Vec<Type> = row
            .iter()
            .enumerate()
            .map(|(i, v)| match v {
                Value::Unit => Type::Unit,
                Value::Int(_) => Type::Int,
                Value::String(_) => Type::String,
                Value::Sort(_) => Type::Sort(match &self.schema[i] {
                    Type::Sort(s) => s.clone(),
                    _ => String::new(),
                }),
            })
            .collect();
        if self.schema != types {
            return Err(format!(
                "expected [{}], found [{}] for {}",
                self.schema
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                types
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                self.name,
            ));
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
                        let (s, c) = sorts.get_mut(s).unwrap().union(old, new)?;
                        changed |= c;
                        Value::Sort(s)
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
        let id = RowId(self.height());
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
    pub fn get_mut(
        &mut self,
        mut xs: Vec<Value>,
        sorts: &mut Sorts,
    ) -> Result<(Option<Value>, bool), String> {
        Ok(if let Some(y) = self.get_ref(&xs) {
            (Some(y), false)
        } else {
            let y = match self.schema.last().unwrap() {
                Type::Sort(sort) => Value::Sort(sorts.get_mut(sort).unwrap().new_key(())),
                Type::Unit => Value::Unit,
                _ => return Ok((None, false)),
            };
            xs.push(y.clone());
            self.insert(xs, sorts)?;
            (Some(y), true)
        })
    }

    /// Get the output in this table with the specific inputs in the input columns.
    /// This method never changes `self`; notably, it will not create new sort elements.
    #[must_use]
    pub fn get_ref(&self, xs: &[Value]) -> Option<Value> {
        assert_eq!(self.width() - 1, xs.len());
        self.get_id(xs).map(|id| {
            get_row(&self.data, &self.schema, id)
                .last()
                .unwrap()
                .clone()
        })
    }

    /// Rebuild the indices so that each `Value::Sort` holds a canonical key.
    /// Does not run to fixpoint, even if rebuilding changes `sorts`.
    pub fn rebuild(
        &mut self,
        sorts: &mut Sorts,
        dirty: &HashMap<String, HashSet<usize>>,
    ) -> Result<bool, String> {
        // For each row containing a dirty value, replace it with its canonicalized version.
        let mut ids = HashSet::new();
        let mut row = vec![None; self.width()];
        for (column, sort) in self.schema.iter().enumerate() {
            if let Type::Sort(sort) = sort {
                for value in &dirty[sort] {
                    row[column] = Some(Value::Sort(*value));
                    ids.extend(self.get_ids(&row));
                    row[column] = None;
                }
            }
        }
        let changed = !ids.is_empty();
        for id in ids {
            self.remove_row(id);
            self.insert(
                get_row(&self.data, &self.schema, id)
                    .iter()
                    .zip(&self.schema)
                    .map(|(v, t)| match (v, t) {
                        (Value::Sort(v), Type::Sort(s)) => {
                            Value::Sort(sorts.get_mut(s).unwrap().find(*v))
                        }
                        _ => v.clone(),
                    })
                    .collect(),
                sorts,
            )?;
        }
        Ok(changed)
    }

    /// Get the set of rows with specific values in specific columns. `None` means "don't care".
    pub fn rows(&self, row: &[Option<Value>], i: Iteration) -> impl Iterator<Item = &[Value]> {
        assert_eq!(self.width(), row.len());
        self.get_ids(row)
            .range(match i {
                Iteration::Past => RowId(0)..self.prev.start,
                Iteration::Prev => self.prev.clone(),
                Iteration::All => RowId(0)..RowId(self.height()),
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
