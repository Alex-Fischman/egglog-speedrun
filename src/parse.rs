use crate::token::*;

pub enum Sexp<'a> {
    Atom(&'a Token<'a>),
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

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Result<Vec<Sexp<'a>>, String> {
    // must not be empty
    type Tokens<'a> = std::iter::Peekable<std::slice::Iter<'a, Token<'a>>>;
    fn parse<'a>(tokens: &mut Tokens<'a>) -> Result<Sexp<'a>, String> {
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
                        Some(_) => list.push(parse(tokens)?),
                    }
                }
            }
            _ => Ok(Sexp::Atom(token)),
        }
    }

    let mut out = vec![];
    let mut tokens = tokens.iter().peekable();
    while tokens.peek().is_some() {
        out.push(parse(&mut tokens)?);
    }
    Ok(out)
}
