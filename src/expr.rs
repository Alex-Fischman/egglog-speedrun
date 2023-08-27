//! This module defines an interpreter for `egglog` expressions.

use crate::*;

/// An `egglog` expression.
#[derive(Clone)]
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
    Sort(usize),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(i) => write!(f, "{i}"),
            Value::Sort(s) => write!(f, "s{s}"),
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

impl Value {
    /// Asserts that this `Value` has the given `Type`.
    pub fn assert_type(&self, t: &Type) -> Result<(), String> {
        // we could do all this by matching on a tuple, but we
        // wouldn't get type errors when adding new `Value` variants
        if match self {
            Value::Unit => matches!(t, Type::Unit),
            Value::Int(_) => matches!(t, Type::Int),
            Value::Sort(_) => matches!(t, Type::Sort(_)),
        } {
            Ok(())
        } else {
            Err(format!("expected {t}, found {self}"))
        }
    }
}

/// A map from variable names to `Value`s.
pub type Vars<'a> = HashMap<&'a str, Value>;
/// A map from function names to `Table`s.
pub type Funcs = HashMap<String, Table>;
/// A map from sort names to `UnionFind`s.
pub type Sorts = HashMap<String, UnionFind<'static, ()>>;

impl Expr {
    /// Get a `Value` from an `Expr` under a given context, allowing database modification.
    pub fn evaluate_mut(
        &self,
        vars: &Vars,
        funcs: &mut Funcs,
        sorts: &mut Sorts,
    ) -> Result<Value, String> {
        self.evaluate_private(vars, &mut |f, xs| {
            Ok(match funcs.get_mut(f) {
                Some(func) => Some(func.get(xs, sorts)?),
                None => None,
            })
        })
    }

    /// Get a `Value` from an `Expr` under a given context, without modifying the database.
    pub fn evaluate_ref(&self, vars: &Vars, funcs: &Funcs) -> Result<Value, String> {
        self.evaluate_private(vars, &mut |f, xs| {
            Ok(funcs.get(f).map(|func| {
                func.rows_with_inputs(&xs)
                    .next()
                    .map(|row| row[row.len() - 1])
            }))
        })
    }

    /// A combined backend for both versions of evaluate, to avoid implementing it twice.
    fn evaluate_private(
        &self,
        vars: &Vars,
        funcs: &mut impl FnMut(&str, Vec<Value>) -> Result<Option<Option<Value>>, String>,
    ) -> Result<Value, String> {
        let int = |expr: &Expr| match expr.evaluate_private(vars, funcs)? {
            Value::Int(i) => Ok(i),
            v => Err(format!("expected {}, found {v}", Type::Int)),
        };
        let ints = |exprs: &[Expr]| exprs.iter().map(int).collect::<Result<Vec<_>, _>>();
        match self {
            Expr::Unit => Ok(Value::Unit),
            Expr::Int(i) => Ok(Value::Int(*i)),
            Expr::Var(s) => match vars.get(s.as_str()) {
                Some(v) => Ok(*v),
                None => Err(format!("unknown variable {self}")),
            },
            Expr::Call(f, xs) => match (f.as_str(), xs.as_slice()) {
                ("+", _) => Ok(Value::Int(ints(xs)?.into_iter().sum())),
                ("min", [_, ..]) => Ok(Value::Int(ints(xs)?.into_iter().min().unwrap())),
                _ => {
                    let xs: Vec<Value> = xs
                        .iter()
                        .map(|x| x.evaluate_private(vars, funcs))
                        .collect::<Result<_, _>>()?;
                    funcs(f, xs)?
                        .ok_or(format!("unknown function {self}"))?
                        .ok_or(format!("unknown value {self}"))
                }
            },
        }
    }
}
