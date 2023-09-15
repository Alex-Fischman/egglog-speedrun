//! This module defines an interpreter for `egglog` expressions.

use crate::*;

/// An `egglog` expression.
#[derive(Clone, Debug)]
pub enum Expr {
    /// The unit value.
    Unit,
    /// An integer.
    Int(i64),
    /// A string.
    String(String),
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
            Expr::String(s) => write!(f, "{s:?}"),
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Value {
    /// The unit value.
    Unit,
    /// An integer.
    Int(i64),
    /// A string.
    String(String),
    /// An element of an uninterpreted sort.
    Sort(usize),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(i) => write!(f, "{i}"),
            Value::String(s) => write!(f, "{s:?}"),
            Value::Sort(s) => write!(f, "s{s}"),
        }
    }
}

/// An `egglog` type.
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// The unit type.
    Unit,
    /// The type of integers (`i64` in `egglog`).
    Int,
    /// The type of strings.
    String,
    /// An uninterpreted sort.
    Sort(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Int => write!(f, "i64"),
            Type::String => write!(f, "String"),
            Type::Sort(s) => write!(f, "{s}"),
        }
    }
}

/// A map from variable names to `Value`s.
pub type Vars<'a> = HashMap<&'a str, Value>;
/// A map from function names to `Table`s.
pub type Funcs = BTreeMap<String, Table>;
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
                Some(func) => Some(func.get_mut(xs, sorts)?.0),
                None => None,
            })
        })?
        .ok_or(format!("unknown value {self}"))
    }

    /// Get a `Value` from an `Expr` under a given context, without modifying the database.
    /// Returns `None` instead of `Err` iff the only error is a lookup failure inside a table.
    pub fn evaluate_ref(&self, vars: &Vars, funcs: &Funcs) -> Result<Option<Value>, String> {
        self.evaluate_private(vars, &mut |f, xs| {
            Ok(funcs.get(f).map(|func| func.get_ref(&xs)))
        })
    }

    /// A combined backend for both versions of evaluate, to avoid implementing it twice.
    // If you add builtin functions, also change Expr::get_type in syntax.rs
    fn evaluate_private(
        &self,
        vars: &Vars,
        funcs: &mut impl FnMut(&str, Vec<Value>) -> Result<Option<Option<Value>>, String>,
    ) -> Result<Option<Value>, String> {
        let int = |expr: &Expr| match expr.evaluate_private(vars, funcs)? {
            Some(Value::Int(i)) => Ok(Some(i)),
            None => Ok(None),
            Some(v) => Err(format!("expected {}, found {v}", Type::Int)),
        };
        let ints = |exprs: &[Expr]| exprs.iter().map(int).collect::<Result<Vec<_>, _>>();
        match self {
            Expr::Unit => Ok(Some(Value::Unit)),
            Expr::Int(i) => Ok(Some(Value::Int(*i))),
            Expr::String(s) => Ok(Some(Value::String(s.clone()))),
            Expr::Var(s) => match vars.get(s.as_str()) {
                Some(v) => Ok(Some(v.clone())),
                None => Err(format!("unknown variable {self}")),
            },
            Expr::Call(f, xs) => match (f.as_str(), xs.as_slice()) {
                ("+", _) => match ints(xs)?.into_iter().try_fold(0, |a, b| Some(a + b?)) {
                    Some(i) => Ok(Some(Value::Int(i))),
                    None => Ok(None),
                },
                ("*", _) => match ints(xs)?.into_iter().try_fold(1, |a, b| Some(a * b?)) {
                    Some(i) => Ok(Some(Value::Int(i))),
                    None => Ok(None),
                },
                ("min", _) => {
                    match ints(xs)?
                        .into_iter()
                        .reduce(|a, b| Some(a?.min(b?)))
                        .ok_or(format!("need at least one argument for {self}"))?
                    {
                        Some(i) => Ok(Some(Value::Int(i))),
                        None => Ok(None),
                    }
                }
                _ => {
                    let xs: Option<Vec<Value>> = xs
                        .iter()
                        .map(|x| x.evaluate_private(vars, funcs))
                        .collect::<Result<_, _>>()?;
                    match xs {
                        Some(xs) => funcs(f, xs)?.ok_or(format!("unknown function {self}")),
                        None => Ok(None),
                    }
                }
            },
        }
    }
}
