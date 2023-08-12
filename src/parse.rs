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
    // `tokens` must not be empty
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
            Sexp::List(_, list) if list.is_empty() => Ok(Expr::Unit),
            Sexp::Atom(token) if token.as_str().parse::<i64>().is_ok() => {
                Ok(Expr::Int(token.as_str().parse::<i64>().unwrap()))
            }
            Sexp::Atom(token) if token.as_str().parse::<f64>().is_ok() => {
                Ok(Expr::Float(token.as_str().parse::<f64>().unwrap()))
            }
            Sexp::Atom(token) => Err(format!("expected expression, found {token}")),
            Sexp::List(token, list) => match list.as_slice() {
                [Sexp::Atom(f), xs @ ..] => Ok(Expr::Call(
                    f.as_str().to_owned(),
                    xs.iter().map(Expr::parse).collect::<Result<_, _>>()?,
                )),
                _ => Err(format!("expected expression, found {token}")),
            },
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
}

impl Type {
    fn parse(sexp: &Sexp) -> Result<Type, String> {
        match sexp {
            Sexp::List(_, list) if list.is_empty() => Ok(Type::Unit),
            Sexp::Atom(token) if token.as_str() == "i64" => Ok(Type::Int),
            Sexp::Atom(token) if token.as_str() == "f64" => Ok(Type::Float),
            Sexp::Atom(token) | Sexp::List(token, _) => {
                Err(format!("expected type, found {token}"))
            }
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
    fn parse(_sexps: &[Sexp]) -> Result<Action<'a>, String> {
        todo!()
    }
}

/// An assertion of equality among expressions.
pub struct Pattern(Vec<Expr>);

impl Pattern {
    fn parse(_sexp: &Sexp) -> Result<Pattern, String> {
        todo!()
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
            Sexp::List(token, list) => match list.get(0) {
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
                    "rule" => match list.as_slice() {
                        [_, Sexp::List(_, patterns), Sexp::List(_, actions)] => Ok(Command::Rule(
                            token,
                            patterns
                                .iter()
                                .map(Pattern::parse)
                                .collect::<Result<_, _>>()?,
                            actions
                                .iter()
                                .map(|sexp| match sexp {
                                    Sexp::List(_, list) => Action::parse(list),
                                    Sexp::Atom(token) => {
                                        Err(format!("expected an action but found {token}"))
                                    }
                                })
                                .collect::<Result<_, _>>()?,
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
                    _ => Ok(Command::Action(Action::parse(&list)?)),
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
            _ => todo!(),
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
