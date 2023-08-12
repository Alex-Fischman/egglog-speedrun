//! This module defines the backend for `egglog`.

use crate::syntax::*;
use std::collections::HashMap;
use std::hash::Hash;

pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
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

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
struct Data([u8; 8]);

#[derive(Clone, PartialEq, Eq, Hash)]
enum Array<T> {
    Arr(usize, [T; ARRAY_SIZE]),
    Vec(Vec<T>),
}

const ARRAY_SIZE: usize = 4;

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

pub struct Table {
    schema: (Array<Type>, Type),
    hashed: HashMap<Array<Data>, usize>,
    linear: Vec<(Array<Data>, Data)>,
}

impl Table {
    fn value_to_data(value: &Value) -> Data {
        match value {
            Value::Unit => Data::default(),
            Value::Int(i) => Data(i.to_le_bytes()),
            Value::Float(f) => Data(f.to_le_bytes()),
            Value::Sort(u) => Data(u.to_le_bytes()),
        }
    }

    fn data_to_value(data: Data, t: &Type) -> Value {
        match t {
            Type::Unit => Value::Unit,
            Type::Int => Value::Int(i64::from_le_bytes(data.0)),
            Type::Float => Value::Float(f64::from_le_bytes(data.0)),
            Type::Sort(_) => Value::Sort(u64::from_le_bytes(data.0)),
        }
    }

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

    #[must_use]
    pub fn new(inputs: Vec<Type>, output: Type) -> Table {
        Table {
            schema: (Array::from(inputs), output),
            hashed: HashMap::default(),
            linear: Vec::default(),
        }
    }

    pub fn get_hashed(&self, inputs: &[Value]) -> Result<Option<usize>, String> {
        self.does_query_match_schema(inputs)?;

        let inputs = Array::from(inputs.iter().map(Table::value_to_data).collect::<Vec<_>>());

        Ok(self.hashed.get(&inputs).copied())
    }

    #[must_use]
    pub fn get_linear(&self, row: usize) -> Option<(Vec<Value>, Value)> {
        self.linear.get(row).map(|t| {
            (
                t.0.iter()
                    .zip(self.schema.0.iter())
                    .map(|t| Table::data_to_value(*t.0, t.1))
                    .collect(),
                Table::data_to_value(t.1, &self.schema.1),
            )
        })
    }

    pub fn insert(&mut self, inputs: &[Value], output: &Value) -> Result<(), String> {
        self.does_query_match_schema(inputs)?;

        let inputs = Array::from(inputs.iter().map(Table::value_to_data).collect::<Vec<_>>());
        let output = Table::value_to_data(output);

        self.hashed.insert(inputs.clone(), self.linear.len());
        self.linear.push((inputs, output));

        Ok(())
    }
}
