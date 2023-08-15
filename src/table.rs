//! This module defines the representation of a `function` in `egglog`.

use crate::*;

/// A single function in an `egglog` program.
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
    /// The rows in this table indexed by each input column.
    columns: Vec<HashMap<Value, RowId>>,
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
        let columns = vec![HashMap::new(); inputs.len()];
        Table {
            name,
            inputs,
            output,
            merge,
            primary: Vec::new(),
            function: HashMap::new(),
            columns,
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
            Some(&row) => {
                let output = self.primary[row].1;
                output.assert_type(&self.output)?;
                Ok(output)
            }
        }
    }

    /// Add a row to the table, merging if a row with the given inputs already exists.
    /// Returns true if the table was changed.
    pub fn insert(&mut self, xs: &[Value], y: Value) -> Result<bool, String> {
        self.match_inputs(xs)?;
        y.assert_type(&self.output)?;
        let append_row = |table: &mut Table| {
            let id = table.primary.len();
            table.primary.push((xs.to_vec(), y, true));
            table.function.insert(xs.to_vec(), id);
            for (column, x) in table.columns.iter_mut().zip(xs) {
                column.insert(*x, id);
            }
            Ok(true)
        };
        match self.function.get(xs) {
            None => append_row(self),
            Some(&row) => {
                let old = self.primary[row].1;
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
                    self.primary[row].2 = false;
                    append_row(self)
                }
            }
        }
    }

    /// Get all of the rows in this table.
    pub fn rows(&self) -> impl Iterator<Item = (&Vec<Value>, &Value)> {
        self.primary
            .iter()
            .filter(|(_, _, live)| *live)
            .map(|(xs, y, _)| (xs, y))
    }
}
