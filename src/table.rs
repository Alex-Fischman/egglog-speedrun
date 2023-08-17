//! This module defines the representation of a `function` in `egglog`.

use crate::*;

/// A single function in an `egglog` program.
#[derive(Clone)]
pub struct Table {
    /// The name of this table in the database.
    name: String,
    /// The schema for the inputs of the `egglog` function.
    inputs: Vec<Type>,
    /// The schema for the output of the `egglog` function.
    output: Type,
    /// The merge function for this table. `None` means "default".
    merge: Option<Expr>,
    /// The data in this table indexed by `RowId`s.
    primary: Vec<(Vec<Value>, Value, bool)>,
    /// The rows in this table indexed by all of the input columns.
    function: HashMap<Vec<Value>, RowId>,
    /// The rows in this table indexed by the values in each column.
    columns: Vec<HashMap<Value, HashSet<RowId>>>,
}

type RowId = usize;

impl Table {
    fn match_inputs(&self, inputs: &[Value]) -> Result<(), String> {
        if inputs.len() == self.inputs.len() {
            for (t, v) in self.inputs.iter().zip(inputs) {
                v.assert_type(t)?;
            }
            Ok(())
        } else {
            Err(format!(
                "expected {} inputs, found {} inputs for {}",
                self.inputs.len(),
                inputs.len(),
                self.name,
            ))
        }
    }

    /// Create a new `Table` with the given schema.
    #[must_use]
    pub fn new(name: String, inputs: Vec<Type>, output: Type, merge: Option<Expr>) -> Table {
        Table {
            primary: Vec::new(),
            function: HashMap::new(),
            columns: vec![HashMap::new(); inputs.len() + 1],
            name,
            inputs,
            output,
            merge,
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
                let output = self.primary[id].1;
                output.assert_type(&self.output)?;
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

    /// Adds a row to the table, assuming that `xs` is not already present.
    fn append_row(&mut self, xs: &[Value], y: Value) {
        let id = self.primary.len();
        self.primary.push((xs.to_vec(), y, true));
        self.function.insert(xs.to_vec(), id);
        for (column, x) in self.columns.iter_mut().zip(xs.iter().chain([&y])) {
            column.entry(*x).or_default().insert(id);
        }
    }

    /// Removes a row from the table by marking it as dead.
    fn remove_row(&mut self, id: RowId) {
        let (xs, y, live) = &mut self.primary[id];
        *live = false;
        self.function.remove(xs);
        for (column, x) in self.columns.iter_mut().zip(xs.iter().chain([&*y])) {
            column.get_mut(x).unwrap().remove(&id);
        }
    }

    /// Add a row to the table, merging if a row with the given inputs already exists.
    /// Returns true if the table was changed.
    pub fn insert(&mut self, xs: &[Value], y: Value) -> Result<bool, String> {
        self.match_inputs(xs)?;
        y.assert_type(&self.output)?;
        match self.function.get(xs) {
            None => {
                self.append_row(xs, y);
                Ok(true)
            }
            Some(&id) => {
                let old = self.primary[id].1;
                let new = match (&self.merge, &self.output) {
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
                    self.append_row(xs, y);
                    Ok(true)
                }
            }
        }
    }

    /// Get all of the rows in this table.
    pub fn rows(&self) -> impl Iterator<Item = (&[Value], Value)> {
        self.primary
            .iter()
            .filter(|(_, _, live)| *live)
            .map(|(xs, y, _)| (xs.as_slice(), *y))
    }

    /// Get all of the rows that have a specific value in a specific column.
    pub fn rows_with_value<'a>(
        &'a self,
        value: Value,
        column: usize,
    ) -> Result<Box<dyn Iterator<Item = (&[Value], Value)> + 'a>, String> {
        let column = self
            .columns
            .get(column)
            .ok_or(format!("invalid column index {column} for {}", self.name))?;
        let iter: Box<dyn Iterator<Item = (&[Value], Value)>> = match column.get(&value) {
            Some(set) => Box::new(
                set.iter()
                    .map(|id| &self.primary[*id])
                    .map(|(xs, y, _)| (xs.as_slice(), *y)),
            ),
            None => Box::new([].into_iter()),
        };
        Ok(iter)
    }
}
