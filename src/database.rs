//! This module defines the backend for `egglog`.

use crate::syntax::*;
use std::collections::HashMap;
use std::hash::Hash;

/// A single value in an `egglog` program.
pub enum Value {
    /// The single element of the `Unit` type.
    Unit,
    /// An integer.
    Int(i64),
    /// A floating-point number.
    Float(f64),
    /// An element of an uninterpreted sort.
    Sort(u64),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Sort(v) => write!(f, "{v}"),
        }
    }
}

/// A compressed representation of a `Value`.
/// The schema for the `Table` contains the information needed to recover the `Value`.
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
struct Data([u8; 8]);

impl From<&Value> for Data {
    fn from(value: &Value) -> Data {
        match value {
            Value::Unit => Data::default(),
            Value::Int(i) => Data(i.to_le_bytes()),
            Value::Float(f) => Data(f.to_le_bytes()),
            Value::Sort(u) => Data(u.to_le_bytes()),
        }
    }
}

impl From<(&Data, &Type)> for Value {
    fn from((data, t): (&Data, &Type)) -> Value {
        match t {
            Type::Unit => Value::Unit,
            Type::Int => Value::Int(i64::from_le_bytes(data.0)),
            Type::Float => Value::Float(f64::from_le_bytes(data.0)),
            Type::Sort(_) => Value::Sort(u64::from_le_bytes(data.0)),
        }
    }
}

/// Can hold a small number of values without allocating.
/// Otherwise, falls back to a `Vec`.
#[derive(Clone, PartialEq, Eq, Hash)]
enum Array<T> {
    Arr(usize, [T; ARRAY_SIZE]),
    Vec(Vec<T>),
}

const ARRAY_SIZE: usize = 3;

impl<T: Default> From<Vec<T>> for Array<T> {
    fn from(vec: Vec<T>) -> Array<T> {
        if vec.len() < ARRAY_SIZE {
            let mut len = 0;
            let mut arr = <[T; ARRAY_SIZE]>::default();
            for x in vec {
                arr[len] = x;
                len += 1;
            }
            Array::Arr(len, arr)
        } else {
            Array::Vec(vec)
        }
    }
}

impl<T> Array<T> {
    fn len(&self) -> usize {
        match self {
            Array::Arr(len, _) => *len,
            Array::Vec(vec) => vec.len(),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &T> {
        match self {
            Array::Arr(_, arr) => arr.iter(),
            Array::Vec(vec) => vec.iter(),
        }
    }
}

/// A single function in an `egglog` program.
pub struct Table {
    schema: (Array<Type>, Type),
    hashed: HashMap<Array<Data>, Row>,
    linear: Vec<(Array<Data>, Data)>,
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
            | (Type::Float, Value::Float(_))
            | (Type::Sort(_), Value::Sort(_)) => Ok(()),
            (t, q) => Err(format!("query value {q} did not match schema type {t}")),
        })
    }

    /// Create a new `Table` with the given schema.
    #[must_use]
    pub fn new(inputs: Vec<Type>, output: Type) -> Table {
        Table {
            schema: (Array::from(inputs), output),
            hashed: HashMap::default(),
            linear: Vec::default(),
        }
    }

    /// Get the index of a row in the table given its inputs.
    pub fn get_hashed(&self, inputs: &[Value]) -> Result<Option<Row>, String> {
        self.does_query_match_schema(inputs)?;

        let inputs = Array::from(inputs.iter().map(Data::from).collect::<Vec<_>>());

        Ok(self.hashed.get(&inputs).copied())
    }

    /// Get a row in the table given its index.
    #[must_use]
    pub fn get_linear(&self, row: Row) -> Option<(Vec<Value>, Value)> {
        self.linear.get(row.0).map(|t| {
            (
                t.0.iter()
                    .zip(self.schema.0.iter())
                    .map(Value::from)
                    .collect(),
                (&t.1, &self.schema.1).into(),
            )
        })
    }

    /// Add a row to the table, using the merge function
    /// if a row with the given inputs already exists.
    pub fn insert(&mut self, inputs: &[Value], output: &Value) -> Result<(), String> {
        self.does_query_match_schema(inputs)?;

        let inputs = Array::from(inputs.iter().map(Data::from).collect::<Vec<_>>());
        let output = output.into();

        match self.hashed.get(&inputs) {
            None => {
                self.hashed.insert(inputs.clone(), Row(self.linear.len()));
                self.linear.push((inputs, output));
            }
            Some(_row) => todo!(), // *old = merge(old, output)
        }

        Ok(())
    }
}
