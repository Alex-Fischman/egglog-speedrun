//! This module defines an interpreter for `egglog` expressions.

pub use std::collections::HashMap;
pub use std::fmt::{Display, Formatter, Result as FmtResult};

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
