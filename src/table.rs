//! This module defines the representation of a `function` in `egglog`.

use crate::*;

/// A single function in an `egglog` program.
pub struct Table {
    inputs: Vec<Type>,
    output: Type,
    merge: Option<Expr>,
    data: HashMap<Vec<Value>, Value>,
}

impl Table {
    fn match_inputs(&self, inputs: &[Value]) -> Result<(), String> {
        if inputs.len() == self.inputs.len() {
            for (t, v) in self.inputs.iter().zip(inputs) {
                v.assert_type(t)?;
            }
            Ok(())
        } else {
            Err(format!(
                "expected {} inputs, found {} inputs",
                self.inputs.len(),
                inputs.len()
            ))
        }
    }

    /// Create a new `Table` with the given schema.
    #[must_use]
    pub fn new(inputs: Vec<Type>, output: Type, merge: Option<Expr>) -> Table {
        let data = HashMap::new();
        Table {
            inputs,
            output,
            merge,
            data,
        }
    }

    /// Get the output of a row in the table given its inputs.
    pub fn get(&self, inputs: &[Value]) -> Result<Value, String> {
        self.match_inputs(inputs)?;
        match self.data.get(inputs) {
            None => Err(String::from("unknown output")),
            Some(&output) => {
                output.assert_type(&self.output)?;
                Ok(output)
            }
        }
    }

    /// Add a row to the table, merging if a row with the given inputs already exists.
    /// Returns true if the table was changed.
    pub fn insert(&mut self, inputs: &[Value], output: Value) -> Result<bool, String> {
        self.match_inputs(inputs)?;
        output.assert_type(&self.output)?;
        match self.data.get_mut(inputs) {
            None => {
                self.data.insert(inputs.to_vec(), output);
                Ok(true)
            }
            Some(in_table) => match (&self.merge, &self.output) {
                (Some(merge), _) => {
                    *in_table = merge.evaluate(
                        &HashMap::from([("old", *in_table), ("new", output)]),
                        &HashMap::new(),
                    )?;
                    Ok(*in_table == output)
                }
                (None, Type::Unit) => Ok(false),
                (None, Type::Int) => Err(String::from("missing merge function")),
                (None, Type::Sort(_)) => todo!("union"),
            },
        }
    }

    /// Get all of the rows in this table.
    pub fn rows(&self) -> impl Iterator<Item = (&Vec<Value>, &Value)> {
        self.data.iter()
    }
}
