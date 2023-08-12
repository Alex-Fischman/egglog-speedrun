//! This module parses `Source`s into `Sexp`s.

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

/// An unrestricted S-expression.
pub enum Sexp<'a> {
    /// A single `Token`.
    Atom(Token<'a>),
    /// A list of S-expressions.
    List(Vec<Sexp<'a>>),
}

impl Display for Sexp<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Sexp::Atom(x) => write!(f, "{}", x.as_str()),
            Sexp::List(v) => {
                write!(f, "(")?;
                if !v.is_empty() {
                    write!(f, "{}", v[0])?;
                    for x in &v[1..] {
                        write!(f, " {}", x)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

/// Parse a source string into S-expressions.
pub fn parse(source: &Source) -> Result<Vec<Sexp>, String> {
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
    let mut out = vec![];
    let mut tokens = tokens.into_iter().map(|(token, _)| token).peekable();
    while tokens.peek().is_some() {
        out.push(parse_sexp(&mut tokens)?);
    }
    Ok(out)
}

// `tokens` must not be empty
fn parse_sexp<'a>(
    tokens: &mut std::iter::Peekable<impl Iterator<Item = Token<'a>>>,
) -> Result<Sexp<'a>, String> {
    let token = tokens.next().unwrap();
    match token.as_str() {
        ")" => Err(format!("extra {}", token)),
        "(" => {
            let mut list = vec![];
            loop {
                match tokens.peek() {
                    None => return Err(String::from("missing )")),
                    Some(token) if token.as_str() == ")" => {
                        tokens.next();
                        return Ok(Sexp::List(list));
                    }
                    Some(_) => list.push(parse_sexp(tokens)?),
                }
            }
        }
        _ => Ok(Sexp::Atom(token)),
    }
}
