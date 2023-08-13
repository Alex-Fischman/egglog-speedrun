//! This module defines the representation of a `function` in `egglog`.

use crate::*;

/// A single function in an `egglog` program.
pub struct Table {
    schema: (Vec<Type>, Type),
    hashed: HashMap<Vec<Value>, Row>,
    linear: Vec<(Vec<Value>, Value)>,
    merger: Expr,
}

/// A wrapper around an index into `Table::linear`.
#[derive(Clone, Copy)]
pub struct Row(usize);

impl Table {
    fn does_query_match_schema(&self, query: &[Value]) -> Result<(), String> {
        if query.len() != self.schema.0.len() {
            return Err(format!(
                "expected {}, found {}",
                self.schema.0.len(),
                query.len()
            ));
        }
        self.schema.0.iter().zip(query).try_for_each(|t| match t {
            (Type::Unit, Value::Unit)
            | (Type::Int, Value::Int(_))
            | (Type::Sort(_), Value::Sort(_)) => Ok(()),
            (t, q) => Err(format!("expected {t}, found {q}")),
        })
    }

    /// Create a new `Table` with the given schema.
    #[must_use]
    pub fn new(inputs: Vec<Type>, output: Type, merger: Expr) -> Table {
        Table {
            schema: (inputs, output),
            hashed: HashMap::default(),
            linear: Vec::default(),
            merger,
        }
    }

    /// Get the index of a row in the table given its inputs.
    pub fn get_hashed(&self, inputs: &[Value]) -> Result<Option<Row>, String> {
        self.does_query_match_schema(inputs)?;
        Ok(self.hashed.get(inputs).copied())
    }

    /// Get a row in the table given its index.
    #[must_use]
    pub fn get_linear(&self, row: Row) -> Option<&(Vec<Value>, Value)> {
        self.linear.get(row.0)
    }

    /// Get the output of a row in the table given its inputs.
    pub fn get_output(&self, inputs: &[Value]) -> Result<Option<Value>, String> {
        match self.get_hashed(inputs)? {
            None => Ok(None),
            Some(row) => match self.get_linear(row) {
                None => unreachable!(),
                Some((_, output)) => Ok(Some(*output)),
            },
        }
    }

    /// Add a row to the table, merging if a row with the given inputs already exists.
    pub fn insert(&mut self, inputs: &[Value], output: Value) -> Result<(), String> {
        self.does_query_match_schema(inputs)?;
        match self.hashed.get(inputs) {
            None => {
                self.hashed.insert(inputs.to_vec(), Row(self.linear.len()));
                self.linear.push((inputs.to_vec(), output));
            }
            Some(row) => {
                let vars = HashMap::from([
                    (String::from("old"), self.linear[row.0].1),
                    (String::from("new"), output),
                ]);
                self.linear[row.0].1 = self.merger.evaluate(&vars, &HashMap::new())?;
            }
        }
        Ok(())
    }
}
