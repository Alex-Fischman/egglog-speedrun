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
    /// The set of all live rows.
    live: BitVec,
    /// The functional index, over all input columns. Contains dead rows.
    function: HashMap<Vec<Value>, RowId>,
    /// The column indices, over each column. Contains dead rows.
    columns: Vec<HashMap<Value, Vec<RowId>>>,
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
    pub fn new(name: String, schema: Vec<Type>, merge: Option<Expr>) -> Table {
        Table {
            columns: vec![HashMap::default(); schema.len()],
            name,
            schema,
            merge,
            data: Vec::new(),
            live: BitVec::default(),
            function: HashMap::default(),
            prev: RowId(0)..RowId(0),
        }
    }

    /// Get the number of rows in this table.
    fn height(&self) -> usize {
        self.data.len() / self.schema.len()
    }

    /// Get the number of live rows in this table.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.live
            .0
            .iter()
            .map(|bits| bits.count_ones() as usize)
            .sum()
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
        if let Some(&id) = self.function.get(&row[..row.len() - 1]) {
            if self.live.get(id) {
                let old = get_row(&self.data, &self.schema, id).last().unwrap();
                let new = row.last_mut().unwrap();
                let mut changed = false;
                *new = match &self.merge {
                    Some(expr) => expr.evaluate_mut(
                        &[("old", old.clone()), ("new", new.clone())]
                            .into_iter()
                            .collect(),
                        &mut BTreeMap::new(),
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
                if old == new {
                    return Ok(changed);
                }
                self.live.set(id, false);
            }
        }

        // Append the new row
        let id = RowId(self.height());
        self.live.set(id, true);
        self.function.insert(row[..row.len() - 1].to_vec(), id);
        for (column, value) in row.iter().enumerate() {
            self.columns[column]
                .entry(value.clone())
                .or_default()
                .push(id);
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
    pub fn get_ref(&self, xs: &[Value]) -> Option<Value> {
        assert_eq!(self.schema.len() - 1, xs.len());
        self.function.get(xs).map(|&id| {
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
        dirty: &HashMap<String, Vec<usize>>,
    ) -> Result<(), String> {
        // For each row containing a dirty value, replace it with its canonicalized version.
        for (column, sort) in self.schema.clone().iter().enumerate() {
            if let Type::Sort(sort) = sort {
                for value in &dirty[sort] {
                    if let Some(vec) = self.columns[column].remove(&Value::Sort(*value)) {
                        for id in vec {
                            if self.live.get(id) {
                                self.live.set(id, false);
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
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn range(&self, i: Iteration) -> Range<RowId> {
        match i {
            Iteration::Past => RowId(0)..self.prev.start,
            Iteration::Prev => self.prev.clone(),
            Iteration::All => RowId(0)..RowId(self.height()),
        }
    }

    /// Get all the live rows from an iteration.
    pub fn rows(&self, i: Iteration) -> impl Iterator<Item = &[Value]> {
        let range = self.range(i);
        (range.start.0..range.end.0)
            .map(RowId) // Step is unstable
            .filter(|&id| self.live.get(id))
            .map(|id| get_row(&self.data, &self.schema, id))
    }

    /// Get the live rows with a specific value in a specific column from an iteration.
    pub fn rows_with_value(
        &self,
        i: Iteration,
        value: &Value,
        column: usize,
    ) -> impl Iterator<Item = &[Value]> {
        let range = self.range(i);
        self.columns[column]
            .get(value)
            .into_iter()
            .flatten()
            .copied()
            .filter(move |&id| self.live.get(id) && range.contains(&id))
            .map(|id| get_row(&self.data, &self.schema, id))
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

/// A subset of the rows in the table, implemented as a bitvector.
#[derive(Clone, Default)]
struct BitVec(Vec<BitVecBacking>);
type BitVecBacking = usize;
const BIT_VEC_BACKING_SIZE: usize = BitVecBacking::BITS as usize;

impl BitVec {
    fn get(&self, RowId(index): RowId) -> bool {
        let i = index / BIT_VEC_BACKING_SIZE;
        let j = index % BIT_VEC_BACKING_SIZE;
        i < self.0.len() && (self.0[i] >> j) & 1 == 1
    }

    fn set(&mut self, RowId(index): RowId, value: bool) {
        let i = index / BIT_VEC_BACKING_SIZE;
        let j = index % BIT_VEC_BACKING_SIZE;
        if i >= self.0.len() {
            self.0.resize(i + 1, 0);
        }
        let mask = 1 << j;
        if value {
            self.0[i] |= mask;
        } else {
            self.0[i] &= !mask;
        }
    }
}
