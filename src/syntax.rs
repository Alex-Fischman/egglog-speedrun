//! This module parses source strings into `egglog` programs.

pub use std::fmt::{Display, Formatter, Result as FmtResult};

/// A `Source` represents a string to be parsed, as well as a name to be used in error messages.
pub struct Source {
    /// The name of this `Source`, to be used in error messages.
    pub name: String,
    /// The text of this `Source`, to be used in `Token`s.
    pub text: String,
}

/// A slice of a `Source` string.
pub struct Token<'a> {
    /// The `Source` that this token comes from.
    pub source: &'a Source,
    range: std::ops::Range<usize>,
}

impl Token<'_> {
    /// Get the string slice that this token holds.
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.source.text[self.range.clone()]
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            match (i, c) {
                (i, _) if i == self.range.start => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        write!(f, "{} at {}:{row}:{col}", self.as_str(), self.source.name)
    }
}

enum Sexp<'a> {
    Atom(Token<'a>),
    List(Token<'a>, Vec<Sexp<'a>>),
}

impl<'a> Sexp<'a> {
    /// Should never be empty.
    fn parse(
        tokens: &mut std::iter::Peekable<impl Iterator<Item = Token<'a>>>,
    ) -> Result<Sexp<'a>, String> {
        let token = tokens.next().unwrap();
        match token.as_str() {
            ")" => Err(format!("extra {token}")),
            "(" => {
                let start = token.range.start;
                let mut list = vec![];
                loop {
                    match tokens.peek() {
                        None => return Err(format!("extra {token}")),
                        Some(token) if token.as_str() == ")" => {
                            let (source, end) = (token.source, token.range.end);
                            tokens.next();
                            return Ok(Sexp::List(
                                Token {
                                    source,
                                    range: start..end,
                                },
                                list,
                            ));
                        }
                        Some(_) => list.push(Sexp::parse(tokens)?),
                    }
                }
            }
            _ => Ok(Sexp::Atom(token)),
        }
    }
}

/// A single value in the abtract syntax tree.
pub enum Expr {
    /// The unit value.
    Unit,
    /// An integer.
    Int(i64),
    /// A floating-point number.
    Float(f64),
    /// A table lookup.
    Call(String, Vec<Expr>),
    /// A reference to a variable.
    Var(String),
}

impl Expr {
    fn parse(sexp: &Sexp) -> Result<Expr, String> {
        match sexp {
            Sexp::List(token, list) => match list.as_slice() {
                [] => Ok(Expr::Unit),
                [Sexp::Atom(f), xs @ ..] => Ok(Expr::Call(
                    f.as_str().to_owned(),
                    xs.iter().map(Expr::parse).collect::<Result<_, _>>()?,
                )),
                _ => Err(format!("expected expression, found {token}")),
            },
            Sexp::Atom(token) => {
                if let Ok(i) = token.as_str().parse::<i64>() {
                    Ok(Expr::Int(i))
                } else if let Ok(f) = token.as_str().parse::<f64>() {
                    Ok(Expr::Float(f))
                } else {
                    Ok(Expr::Var(token.as_str().to_owned()))
                }
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Expr::Unit => write!(f, "()"),
            Expr::Int(i) => write!(f, "{i}"),
            Expr::Float(f_) => write!(f, "{f_}"),
            Expr::Call(f_, xs) => write!(
                f,
                "({f_} {})",
                xs.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Expr::Var(s) => write!(f, "{s}"),
        }
    }
}

/// The type of an `Expr`.
pub enum Type {
    /// The unit type.
    Unit,
    /// The type of integers (`i64` in `egglog`).
    Int,
    /// The type of floating-point numbers (`f64` in `egglog`).
    Float,
    /// An uninterpreted sort.
    Sort(String),
}

impl Type {
    fn parse(sexp: &Sexp) -> Result<Type, String> {
        match sexp {
            Sexp::List(_, list) if list.is_empty() => Ok(Type::Unit),
            Sexp::List(token, _) => Err(format!("expected type, found {token}")),
            Sexp::Atom(token) if token.as_str() == "i64" => Ok(Type::Int),
            Sexp::Atom(token) if token.as_str() == "f64" => Ok(Type::Float),
            Sexp::Atom(token) => Ok(Type::Sort(token.as_str().to_owned())),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Int => write!(f, "i64"),
            Type::Float => write!(f, "f64"),
            Type::Sort(s) => write!(f, "{s}"),
        }
    }
}

/// An action, either as a top-level `Command` or in the head of a rule.
/// Each variant holds a `Token` for error reporting.
pub enum Action<'a> {
    /// Add a row to table `f`, merging if necessary.
    Insert(Token<'a>, String, Vec<Expr>, Expr),
}

impl<'a> Action<'a> {
    fn parse(sexp: Sexp<'a>) -> Result<Action<'a>, String> {
        match sexp {
            Sexp::List(token, list) => match list.get(0) {
                Some(Sexp::Atom(action)) => match action.as_str() {
                    "set" => match list.as_slice() {
                        [_, Sexp::Atom(f), Sexp::List(_, xs), y] => Ok(Action::Insert(
                            token,
                            f.as_str().to_owned(),
                            xs.iter().map(Expr::parse).collect::<Result<_, _>>()?,
                            Expr::parse(y)?,
                        )),
                        _ => Err(format!("expected `set` action, found {token}")),
                    },
                    f => match list[1..].iter().map(Expr::parse).collect::<Result<_, _>>() {
                        Ok(xs) => Ok(Action::Insert(token, f.to_owned(), xs, Expr::Unit)),
                        Err(_) => Err(format!("expected action, found {token}")),
                    },
                },
                _ => Err(format!("expected action, found {token}")),
            },
            Sexp::Atom(token) => Err(format!("expected action, found {token}")),
        }
    }
}

impl Display for Action<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Action::Insert(_, f_, xs, y) => write!(
                f,
                "(set ({f_} {}) {y})",
                xs.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

/// An assertion of equality among expressions.
/// Should never be empty.
pub struct Pattern(Vec<Expr>);

impl Pattern {
    fn parse(sexp: &Sexp) -> Result<Pattern, String> {
        match sexp {
            Sexp::List(_, list) => match list.as_slice() {
                [Sexp::Atom(eq), rest @ ..] if eq.as_str() == "=" => Ok(Pattern(
                    rest.iter().map(Expr::parse).collect::<Result<_, _>>()?,
                )),
                _ => Ok(Pattern(vec![Expr::parse(sexp)?])),
            },
            Sexp::Atom(_) => Ok(Pattern(vec![Expr::parse(sexp)?])),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if self.0.len() == 1 {
            write!(f, "{}", self.0[0])
        } else {
            write!(
                f,
                "(= {})",
                self.0
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        }
    }
}

/// A top-level command.
/// Each variant holds a `Token` for error reporting.
pub enum Command<'a> {
    /// Create a new uninterpreted sort.
    Sort(Token<'a>, String),
    /// Create a new function.
    Function(Token<'a>, String, Vec<Type>, Type),
    /// Create a rule, which performs the actions in the `head`
    /// if all the patterns in the `body` are matched.
    Rule(Token<'a>, Vec<Pattern>, Vec<Action<'a>>),
    /// Run the `egglog` program.
    Run(Token<'a>),
    /// Get the value of a given `Expr`.
    Check(Token<'a>, Expr),
    /// Run an action.
    Action(Action<'a>),
}

impl<'a> Command<'a> {
    fn parse(sexp: Sexp<'a>) -> Result<Command<'a>, String> {
        match sexp {
            Sexp::List(token, mut list) => match list.get(0) {
                Some(Sexp::Atom(command)) => match command.as_str() {
                    "sort" => match list.as_slice() {
                        [_, Sexp::Atom(sort)] => Ok(Command::Sort(token, sort.as_str().to_owned())),
                        _ => Err(format!("expected `sort` command, found {token}")),
                    },
                    "function" => match list.as_slice() {
                        [_, Sexp::Atom(f), Sexp::List(_, xs), y] => Ok(Command::Function(
                            token,
                            f.as_str().to_owned(),
                            xs.iter().map(Type::parse).collect::<Result<_, _>>()?,
                            Type::parse(y)?,
                        )),
                        _ => Err(format!("expected `function` command, found {token}")),
                    },
                    "relation" => match list.as_slice() {
                        [_, Sexp::Atom(f), Sexp::List(_, xs)] => Ok(Command::Function(
                            token,
                            f.as_str().to_owned(),
                            xs.iter().map(Type::parse).collect::<Result<_, _>>()?,
                            Type::Unit,
                        )),
                        _ => Err(format!("expected `relation` command, found {token}")),
                    },
                    "rule" => match list.as_slice() {
                        [_, Sexp::List(_, patterns), Sexp::List(..)] => Ok(Command::Rule(
                            token,
                            patterns
                                .iter()
                                .map(Pattern::parse)
                                .collect::<Result<_, _>>()?,
                            match list.remove(2) {
                                Sexp::List(_, actions) => actions
                                    .into_iter()
                                    .map(Action::parse)
                                    .collect::<Result<_, _>>()?,
                                Sexp::Atom(_) => unreachable!(),
                            },
                        )),
                        _ => Err(format!("expeted `rule` command, found {token}")),
                    },
                    "run" => match list.as_slice() {
                        [_] => Ok(Command::Run(token)),
                        _ => Err(format!("expeted `run` command, found {token}")),
                    },
                    "check" => match list.as_slice() {
                        [_, expr] => Ok(Command::Check(token, Expr::parse(expr)?)),
                        _ => Err(format!("expeted `check` command, found {token}")),
                    },
                    _ => Ok(Command::Action(Action::parse(Sexp::List(token, list))?)),
                },
                _ => Err(format!("expected command, found {token}")),
            },
            Sexp::Atom(token) => Err(format!("expected command, found {token}")),
        }
    }
}

impl Display for Command<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Command::Sort(_, sort) => write!(f, "(sort {sort})"),
            Command::Function(_, f_, xs, y) => write!(
                f,
                "(function {f_} ({}) {y})",
                xs.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Command::Rule(_, ps, qs) => write!(
                f,
                "(rule ({}) ({}))",
                ps.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" "),
                qs.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Command::Run(_) => write!(f, "(run)"),
            Command::Check(_, x) => write!(f, "(check {x})"),
            Command::Action(q) => write!(f, "{q}"),
        }
    }
}

/// Parse a source string into an `egglog` program.
/// # Errors
/// Will return an error if the source text could not be parsed.
pub fn parse(source: &Source) -> Result<Vec<Command>, String> {
    // split source text into tokens
    let mut tokens: Vec<(Token, bool)> = vec![];
    for (i, c) in source.text.char_indices() {
        match c {
            '(' | ')' => tokens.push((
                Token {
                    source,
                    range: i..i + 1,
                },
                false,
            )),
            c if c.is_whitespace() => {}
            _ => match tokens.last_mut() {
                Some((token, is_atom)) if token.range.end == i && *is_atom => token.range.end += 1,
                _ => tokens.push((
                    Token {
                        source,
                        range: i..i + 1,
                    },
                    true,
                )),
            },
        }
    }

    // parse token stream into sexps
    let mut sexps = vec![];
    let mut tokens = tokens.into_iter().map(|(token, _)| token).peekable();
    while tokens.peek().is_some() {
        sexps.push(Sexp::parse(&mut tokens)?);
    }

    // translate sexps into commands
    sexps.into_iter().map(Command::parse).collect()
}
