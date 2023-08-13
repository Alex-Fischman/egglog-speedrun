//! This module defines an interpreter for `egglog` expressions.

use crate::*;

/// An `egglog` expression.
pub enum Expr {
    /// The unit value.
    Unit,
    /// An integer.
    Int(i64),
    /// A variable.
    Var(String),
    /// A table lookup.
    Call(String, Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Expr::Unit => write!(f, "()"),
            Expr::Int(i) => write!(f, "{i}"),
            Expr::Var(s) => write!(f, "{s}"),
            Expr::Call(f_, xs) => write!(
                f,
                "({f_} {})",
                xs.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

/// An `egglog` value.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    /// The unit value.
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

/// An `egglog` type.
pub enum Type {
    /// The unit type.
    Unit,
    /// The type of integers (`i64` in `egglog`).
    Int,
    /// An uninterpreted sort.
    Sort(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Int => write!(f, "i64"),
            Type::Sort(s) => write!(f, "{s}"),
        }
    }
}

impl Expr {
    /// Get a `Value` from an `Expr` under a given context.
    pub fn evaluate(
        &self,
        vars: &HashMap<String, Value>,
        funcs: &HashMap<String, Table>,
    ) -> Result<Value, String> {
        let int = |expr: &Expr| match expr.evaluate(vars, funcs)? {
            Value::Int(i) => Ok(i),
            v => Err(format!("expected {}, found {v}", Type::Int)),
        };
        let ints = |exprs: &[Expr]| exprs.iter().map(int).collect::<Result<Vec<_>, _>>();
        match self {
            Expr::Unit => Ok(Value::Unit),
            Expr::Int(i) => Ok(Value::Int(*i)),
            Expr::Var(s) => match vars.get(s) {
                Some(v) => Ok(*v),
                None => Err(format!("unknown variable {self}")),
            },
            Expr::Call(f, xs) => match (f.as_str(), xs.as_slice()) {
                ("add", _) => Ok(Value::Int(ints(xs)?.into_iter().sum())),
                ("min", [_, ..]) => Ok(Value::Int(ints(xs)?.into_iter().min().unwrap())),
                _ => match funcs.get(f) {
                    Some(func) => Ok(func
                        .get_output(
                            &xs.iter()
                                .map(|x| x.evaluate(vars, funcs))
                                .collect::<Result<Vec<_>, _>>()?,
                        )?
                        .unwrap()),
                    None => Err(format!("unknown function {self}")),
                },
            },
        }
    }
}
