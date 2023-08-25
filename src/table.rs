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
    fn inputs(&self) -> &[Type] {
        &self.schema[..self.schema.len() - 1]
    }

    fn output(&self) -> &Type {
        &self.schema[self.schema.len() - 1]
    }

    fn match_inputs(&self, inputs: &[Value]) -> Result<(), String> {
        if inputs.len() == self.inputs().len() {
            for (t, v) in self.inputs().iter().zip(inputs) {
                v.assert_type(t)?;
            }
            Ok(())
        } else {
            Err(format!(
                "expected {} inputs, found {} inputs for {}",
                self.inputs().len(),
                inputs.len(),
                self.name,
            ))
        }
    }

    fn match_output(&self, output: Value) -> Result<(), String> {
        output.assert_type(self.output())
    }

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

    /// Get the output of a row in the table given its inputs.
    pub fn get(&self, xs: &[Value]) -> Result<Value, String> {
        self.match_inputs(xs)?;
        match self.function.get(xs) {
            None => Err(format!(
                "unknown output for ({} {})",
                self.name,
                xs.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            )),
            Some(&id) => {
                assert_eq!(xs, &self.primary[id].0[..self.schema.len() - 1]);
                let output = self.primary[id].0[self.schema.len() - 1];
                self.match_output(output)?;
                Ok(output)
            }
        }
    }

    /// Get the number of live rows in the table.
    #[allow(clippy::len_without_is_empty)]
    #[must_use]
    pub fn len(&self) -> usize {
        self.function.len()
    }

    /// Adds a row to the table, without checking if the inputs are already present.
    fn append_row(&mut self, row: &[Value]) {
        let id = self.primary.len();
        self.primary.push((row.to_vec(), true));
        self.function.insert(row[..row.len() - 1].to_vec(), id);
        for (column, x) in self.columns.iter_mut().zip(row) {
            column.entry(*x).or_default().insert(id);
        }
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
    pub fn insert(&mut self, xs: &[Value], y: Value) -> Result<bool, String> {
        self.match_inputs(xs)?;
        self.match_output(y)?;
        match self.function.get(xs) {
            None => {
                let mut row = xs.to_vec();
                row.push(y);
                self.append_row(&row);
                Ok(true)
            }
            Some(&id) => {
                let old = self.primary[id].0[self.schema.len() - 1];
                let new = match (&self.merge, &self.output()) {
                    (Some(merge), _) => merge
                        .evaluate(&HashMap::from([("old", old), ("new", y)]), &HashMap::new())?,
                    (None, Type::Unit) => Value::Unit,
                    (None, Type::Int) => {
                        return Err(format!("missing merge function for {}", self.name))
                    }
                    (None, Type::Sort(_)) => todo!("union-find get/make-set"),
                };
                if old == new {
                    Ok(false)
                } else {
                    self.remove_row(id);
                    let mut row = xs.to_vec();
                    row.push(new);
                    self.append_row(&row);
                    Ok(true)
                }
            }
        }
    }

    /// Get all of the rows in this table.
    pub fn rows(&self) -> impl Iterator<Item = &[Value]> {
        self.primary
            .iter()
            .filter(|(_, live)| *live)
            .map(|(row, _)| row.as_slice())
    }

    /// Get all of the rows that have a specific value in a specific column.
    pub fn rows_with_value_in_column<'a>(
        &'a self,
        value: Value,
        column: usize,
    ) -> Result<Box<dyn Iterator<Item = &[Value]> + 'a>, String> {
        let column = self
            .columns
            .get(column)
            .ok_or(format!("invalid column index {column} for {}", self.name))?;
        let iter: Box<dyn Iterator<Item = &[Value]>> = match column.get(&value) {
            Some(set) => Box::new(
                set.iter()
                    .map(|id| &self.primary[*id])
                    .map(|(row, _)| row.as_slice()),
            ),
            None => Box::new(std::iter::empty()),
        };
        Ok(iter)
    }
}
