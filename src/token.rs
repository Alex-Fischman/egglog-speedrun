pub use std::fmt::{Display, Formatter, Result as FmtResult};

pub struct Token<'a> {
    pub source: &'a Source,
    range: std::ops::Range<usize>,
}

pub struct Source {
    pub name: String,
    pub text: String,
}

impl Token<'_> {
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

pub fn tokenize(source: &Source) -> Vec<Token> {
    let mut tokens = vec![];
    for (i, c) in source.text.char_indices() {
        match c {
            '(' | ')' => tokens.push(Token {
                source,
                range: i..i + 1,
            }),
            c if c.is_whitespace() => {}
            _ => match tokens.last_mut() {
                Some(token) if token.range.end == i && !"()".contains(token.as_str()) => {
                    token.range.end += 1
                }
                _ => tokens.push(Token {
                    source,
                    range: i..i + 1,
                }),
            },
        }
    }
    tokens
}
