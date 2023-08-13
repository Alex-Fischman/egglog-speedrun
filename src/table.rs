//! This module defines the backend for `egglog`.

use crate::syntax::*;
use std::collections::HashMap;

/// A single value in an `egglog` program.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    /// The single element of the `Unit` type.
    Unit,
    /// An integer.
    Int(i64),
    /// An element of an uninterpreted sort.
    Sort(u64),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(v) => write!(f, "{v}"),
            Value::Sort(v) => write!(f, "{v}"),
        }
    }
}

/// A single function in an `egglog` program.
pub struct Table {
    schema: (Vec<Type>, Type),
    hashed: HashMap<Vec<Value>, Row>,
    linear: Vec<(Vec<Value>, Value)>,
}

/// A wrapper around an index into `Table::linear`.
#[derive(Clone, Copy)]
pub struct Row(usize);

impl Table {
    fn does_query_match_schema(&self, query: &[Value]) -> Result<(), String> {
        if query.len() != self.schema.0.len() {
            return Err(format!(
                "query length {} did not match schema length {}",
                query.len(),
                self.schema.0.len()
            ));
        }
        self.schema.0.iter().zip(query).try_for_each(|t| match t {
            (Type::Unit, Value::Unit)
            | (Type::Int, Value::Int(_))
            | (Type::Sort(_), Value::Sort(_)) => Ok(()),
            (t, q) => Err(format!("query value {q} did not match schema type {t}")),
        })
    }

    /// Create a new `Table` with the given schema.
    #[must_use]
    pub fn new(inputs: Vec<Type>, output: Type) -> Table {
        Table {
            schema: (inputs, output),
            hashed: HashMap::default(),
            linear: Vec::default(),
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

    /// Add a row to the table, using the merge function
    /// if a row with the given inputs already exists.
    pub fn insert(&mut self, inputs: &[Value], output: Value) -> Result<(), String> {
        self.does_query_match_schema(inputs)?;
        match self.hashed.get(inputs) {
            None => {
                self.hashed.insert(inputs.to_vec(), Row(self.linear.len()));
                self.linear.push((inputs.to_vec(), output));
            }
            Some(_row) => todo!(),
        }
        Ok(())
    }
}
