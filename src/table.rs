//! This module defines the representation of a `function` in `egglog`.

use crate::*;

/// A single function in an `egglog` program.
#[derive(Clone)]
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
    #[allow(clippy::len_without_is_empty)]
    #[must_use]
    pub fn len(&self) -> usize {
        self.function.len()
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
    pub fn insert(&mut self, mut row: Vec<Value>) -> Result<bool, String> {
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
                Some(expr) => expr.evaluate(
                    &HashMap::from([("old", *old), ("new", *new)]),
                    &HashMap::new(),
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

    /// Get all of the rows in this table.
    pub fn rows(&self) -> impl Iterator<Item = &[Value]> {
        self.primary
            .iter()
            .filter(|(_, live)| *live)
            .map(|(row, _)| row.as_slice())
    }

    /// Get the (at most) one row in this table with the specific inputs in the input columns.
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
