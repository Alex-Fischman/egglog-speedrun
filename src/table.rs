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
    func: RawTable<RowId>,
    /// The column indices, over each column. Contains dead rows.
    columns: Vec<HashMap<Value, Vec<RowId>>>,
    /// The rows that were added in the last iteration.
    prev: Range<RowId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct RowId(usize);

impl RowId {
    /// Returns all of the values in a given row.
    fn get_row<'a>(self, data: &'a [Value], schema: &[Type]) -> &'a [Value] {
        &data[self.0 * schema.len()..(self.0 + 1) * schema.len()]
    }

    /// Returns the input values in a given row.
    fn get_xs<'a>(self, data: &'a [Value], schema: &[Type]) -> &'a [Value] {
        &data[self.0 * schema.len()..(self.0 + 1) * schema.len() - 1]
    }

    /// Returns the input values in a given row.
    fn get_y<'a>(self, data: &'a [Value], schema: &[Type]) -> &'a Value {
        &data[(self.0 + 1) * schema.len() - 1]
    }
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
            func: RawTable::new(),
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

    fn hash(xs: &[Value]) -> u64 {
        let mut hasher = BuildHasherDefault::<FxHasher>::default().build_hasher();
        xs.hash(&mut hasher);
        hasher.finish()
    }

    fn get_id(&self, xs: &[Value]) -> Option<&RowId> {
        self.func.get(Table::hash(xs), |&id| {
            xs == id.get_xs(&self.data, &self.schema)
        })
    }

    /// Add a row to the table, merging if a row with the given inputs already exists.
    /// Returns true if the either the table or `sorts` was changed.
    pub fn insert(
        &mut self,
        xs: &[Value],
        mut y: Value,
        sorts: &mut Sorts,
    ) -> Result<bool, String> {
        let types: Vec<Type> = xs
            .iter()
            .chain([&y])
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
        if let Some(&id) = self.get_id(xs) {
            if self.live.get(id) {
                let old = id.get_y(&self.data, &self.schema);
                let new = &mut y;
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
        unsafe {
            match self.func.find_or_find_insert_slot(
                Table::hash(xs),
                |&id| xs == id.get_xs(&self.data, &self.schema),
                |&id| Table::hash(id.get_xs(&self.data, &self.schema)),
            ) {
                Ok(bucket) => {
                    // Only replace dead rows
                    assert!(!self.live.get(*bucket.as_ref()));
                    *bucket.as_mut() = id;
                }
                Err(slot) => {
                    // SAFETY: no mutation has occured since the call to find_or_find_insert_slot
                    self.func.insert_in_slot(Table::hash(xs), slot, id);
                }
            }
        }
        for (column, value) in xs.iter().chain([&y]).enumerate() {
            self.columns[column]
                .entry(value.clone())
                .or_default()
                .push(id);
        }
        self.data.extend(xs.iter().cloned());
        self.data.push(y);
        Ok(true)
    }

    /// Get the output value of a row for some inputs. If there isn't a value, but the
    /// output type is a sort, create a new sort element, add it to the table, and return it.
    pub fn get_mut(
        &mut self,
        xs: &[Value],
        sorts: &mut Sorts,
    ) -> Result<(Option<Value>, bool), String> {
        let changed = if self.get_id(xs).is_some() {
            false
        } else {
            self.insert(
                xs,
                match self.schema.last().unwrap() {
                    Type::Sort(sort) => Value::Sort(sorts.get_mut(sort).unwrap().new_key(())),
                    Type::Unit => Value::Unit,
                    _ => return Ok((None, false)),
                },
                sorts,
            )?;
            true
        };
        Ok((self.get_ref(xs), changed))
    }

    /// Get the output in this table with the specific inputs in the input columns.
    /// This method never changes `self`; notably, it will not create new sort elements.
    pub fn get_ref(&self, xs: &[Value]) -> Option<Value> {
        assert_eq!(self.schema.len() - 1, xs.len());
        self.get_id(xs)
            .map(|id| id.get_y(&self.data, &self.schema).clone())
    }

    /// Rebuild the indices so that each `Value::Sort` holds a canonical key.
    /// Does not run to fixpoint, even if rebuilding changes `sorts`.
    pub fn rebuild(
        &mut self,
        sorts: &mut Sorts,
        dirty: &HashMap<String, Vec<usize>>,
    ) -> Result<(), String> {
        fn rebuild_row(
            id: RowId,
            table: &mut Table,
            sorts: &mut Sorts,
            row: &mut Vec<Value>,
        ) -> Result<(), String> {
            if table.live.get(id) {
                table.live.set(id, false);
                row.clear();
                row.extend_from_slice(id.get_row(&table.data, &table.schema));
                for (v, t) in row.iter_mut().zip(&table.schema) {
                    if let (Value::Sort(v), Type::Sort(s)) = (v, t) {
                        *v = sorts.get_mut(s).unwrap().find(*v);
                    }
                }
                let y = row.pop().unwrap();
                table.insert(row, y, sorts)?;
            }
            Ok(())
        }

        // Loop-hoist the allocation for the new row.
        // (I think the optimizer does this already but just to be safe I do it explicitly.)
        let mut row = Vec::new();
        // For each row containing a dirty value, replace it with its canonicalized version.
        for (column, sort) in self.schema.clone().iter().enumerate() {
            if let Type::Sort(sort) = sort {
                for value in &dirty[sort] {
                    if let Some(vec) = self.columns[column].remove(&Value::Sort(*value)) {
                        for id in vec {
                            rebuild_row(id, self, sorts, &mut row)?;
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
    pub fn rows(&self, iteration: Iteration) -> impl Iterator<Item = &[Value]> {
        let range = self.range(iteration);
        (range.start.0..range.end.0)
            .map(RowId) // Step is unstable
            .filter(|&id| self.live.get(id))
            .map(|id| id.get_row(&self.data, &self.schema))
    }

    /// Get the live rows with a specific value in a specific column from an iteration.
    pub fn rows_with_value(
        &self,
        iteration: Iteration,
        value: &Value,
        column: usize,
    ) -> impl Iterator<Item = &[Value]> {
        let range = self.range(iteration);
        self.columns[column]
            .get(value)
            .into_iter()
            .flatten()
            .copied()
            .filter(move |&id| self.live.get(id) && range.contains(&id))
            .map(|id| id.get_row(&self.data, &self.schema))
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
