//! This module defines the representation of a `function` in `egglog`.

use crate::*;

/// A single function in an `egglog` program.
pub struct Table {
    inputs: Vec<Type>,
    output: Type,
    merge: Expr,
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
    pub fn new(inputs: Vec<Type>, output: Type, merge: Expr) -> Table {
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
    pub fn insert(&mut self, inputs: &[Value], output: Value) -> Result<(), String> {
        self.match_inputs(inputs)?;
        output.assert_type(&self.output)?;
        match self.data.get_mut(inputs) {
            None => {
                self.data.insert(inputs.to_vec(), output);
            }
            Some(old) => {
                let vars =
                    HashMap::from([(String::from("old"), *old), (String::from("new"), output)]);
                *old = self.merge.evaluate(&vars, &HashMap::new())?;
            }
        }
        Ok(())
    }
}
