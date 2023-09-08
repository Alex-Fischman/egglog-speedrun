//! This module parses source strings into `egglog` programs.

use crate::*;

/// A `Source` represents a string to be parsed, as well as a name to be used in error messages.
pub struct Source {
    /// The name of this `Source`, to be used in error messages.
    pub name: String,
    /// The text of this `Source`, to be used in `Slice`s.
    pub text: String,
}

/// A slice of a `Source` string.
pub struct Slice<'a> {
    /// The `Source` that this slice comes from.
    pub source: &'a Source,
    range: std::ops::Range<usize>,
}

impl Slice<'_> {
    /// Get the string slice that this slice holds.
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.source.text[self.range.clone()]
    }
}

impl Display for Slice<'_> {
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
    Atom(Slice<'a>),
    List(Slice<'a>, Vec<Sexp<'a>>),
}

/// A pattern to match on in the body of a `Command::Rule`.
/// It represents an assertion of equality between its `Expr`s.
/// `self.0` must be nonempty.
pub struct Pattern(pub Vec<Expr>);

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
                    .join(" "),
            )
        }
    }
}

/// An action, either as a top-level `Command` or in the head of a `Command::Rule`.
/// Each variant holds a `Slice` for error reporting.
pub enum Action<'a> {
    /// Add a row to table `f`, merging if necessary.
    Insert(Slice<'a>, String, Vec<Expr>, Expr),
    /// Union two elements of a sort together.
    Union(Expr, Expr, String),
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
            Action::Union(x, y, _) => write!(f, "(union {x} {y})"),
        }
    }
}

/// A top-level command.
/// Each variant holds a `Slice` for error reporting.
pub enum Command<'a> {
    /// Create a new uninterpreted sort.
    Sort(Slice<'a>, String),
    /// Create a new function, with a name, input types,
    /// an output type, and possibly a merge expression.
    Function(Slice<'a>, String, Vec<Type>, Type, Option<Expr>),
    /// Create a rule, which performs the actions in the `head`
    /// if all the patterns in the `body` are matched.
    Rule(Slice<'a>, Vec<Pattern>, Vec<Action<'a>>),
    /// Run the `egglog` program.
    Run(Slice<'a>),
    /// Get the value of a given `Expr`.
    Check(Slice<'a>, Vec<Pattern>),
    /// Run an action.
    Action(Action<'a>),
}

impl Display for Command<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Command::Sort(_, sort) => write!(f, "(sort {sort})"),
            Command::Function(_, f_, xs, y, merge) => {
                let xs = xs
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "(function {f_} ({xs}) {y}")?;
                if let Some(merge) = merge {
                    write!(f, " :merge {merge}")?;
                }
                write!(f, ")")
            }
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
            Command::Check(_, ps) => write!(
                f,
                "(check {})",
                ps.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Command::Action(q) => write!(f, "{q}"),
        }
    }
}

impl<'a> Sexp<'a> {
    /// Should never be empty.
    fn parse(
        slices: &mut std::iter::Peekable<impl Iterator<Item = Slice<'a>>>,
    ) -> Result<Sexp<'a>, String> {
        let slice = slices.next().unwrap();
        match slice.as_str() {
            ")" => Err(format!("extra {slice}")),
            "(" => {
                let start = slice.range.start;
                let mut list = Vec::new();
                loop {
                    match slices.peek() {
                        None => return Err(format!("extra {slice}")),
                        Some(slice) if slice.as_str() == ")" => {
                            let (source, end) = (slice.source, slice.range.end);
                            slices.next();
                            return Ok(Sexp::List(
                                Slice {
                                    source,
                                    range: start..end,
                                },
                                list,
                            ));
                        }
                        Some(_) => list.push(Sexp::parse(slices)?),
                    }
                }
            }
            _ => Ok(Sexp::Atom(slice)),
        }
    }

    fn to_expr(&self) -> Result<Expr, String> {
        match self {
            Sexp::List(slice, list) => match list.as_slice() {
                [] => Ok(Expr::Unit),
                [Sexp::Atom(f), xs @ ..] => Ok(Expr::Call(
                    f.as_str().to_owned(),
                    xs.iter().map(Sexp::to_expr).collect::<Result<_, _>>()?,
                )),
                _ => Err(format!("expected expression, found {slice}")),
            },
            Sexp::Atom(slice) => {
                if let Ok(i) = slice.as_str().parse::<i64>() {
                    Ok(Expr::Int(i))
                } else if let Some(s) = parse_to_string(slice)? {
                    Ok(Expr::String(s))
                } else {
                    Ok(Expr::Var(slice.as_str().to_owned()))
                }
            }
        }
    }

    fn to_type(&self) -> Result<Type, String> {
        match self {
            Sexp::List(_, list) if list.is_empty() => Ok(Type::Unit),
            Sexp::List(slice, _) => Err(format!("expected type, found {slice}")),
            Sexp::Atom(slice) if slice.as_str() == "i64" => Ok(Type::Int),
            Sexp::Atom(slice) if slice.as_str() == "String" => Ok(Type::String),
            Sexp::Atom(slice) => Ok(Type::Sort(slice.as_str().to_owned())),
        }
    }

    fn to_pattern(&self) -> Result<Pattern, String> {
        match self {
            Sexp::List(_, list) => match list.as_slice() {
                [Sexp::Atom(eq), args @ ..] if eq.as_str() == "=" => Ok(Pattern(
                    args.iter().map(Sexp::to_expr).collect::<Result<_, _>>()?,
                )),
                _ => Ok(Pattern(vec![self.to_expr()?])),
            },
            Sexp::Atom(slice) => Err(format!("expected pattern, found {slice}")),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn to_action(self, funcs: &HashMap<String, Type>) -> Result<Action<'a>, String> {
        match self {
            Sexp::List(slice, list) => match list.get(0) {
                Some(Sexp::Atom(action)) => match action.as_str() {
                    "set" => match list.as_slice() {
                        [_, call, y] => match call.to_expr()? {
                            Expr::Call(f, xs) => Ok(Action::Insert(slice, f, xs, y.to_expr()?)),
                            _ => Err(format!("expected `set` action, found {slice}")),
                        },
                        _ => Err(format!("expected `set` action, found {slice}")),
                    },
                    "union" => match list.as_slice() {
                        [_, x, y] => {
                            let (x, y) = (x.to_expr()?, y.to_expr()?);
                            match (x.get_type(funcs)?, y.get_type(funcs)?) {
                                (Type::Sort(sx), Type::Sort(sy)) if sx == sy => {
                                    Ok(Action::Union(x, y, sx))
                                }
                                _ => Err(format!("expected matching sorts, found {slice}")),
                            }
                        }
                        _ => Err(format!("expected `union` action, found {slice}")),
                    },
                    f => match list[1..]
                        .iter()
                        .map(Sexp::to_expr)
                        .collect::<Result<_, _>>()
                    {
                        Ok(xs) => Ok(Action::Insert(slice, f.to_owned(), xs, Expr::Unit)),
                        Err(_) => Err(format!("expected action, found {slice}")),
                    },
                },
                _ => Err(format!("expected action, found {slice}")),
            },
            Sexp::Atom(slice) => Err(format!("expected action, found {slice}")),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn to_commands(self, funcs: &HashMap<String, Type>) -> Result<Vec<Command<'a>>, String> {
        match self {
            Sexp::List(slice, mut list) => match list.get(0) {
                Some(Sexp::Atom(command)) => match command.as_str() {
                    "sort" => match list.as_slice() {
                        [_, Sexp::Atom(sort)] => {
                            Ok(vec![Command::Sort(slice, sort.as_str().to_owned())])
                        }
                        _ => Err(format!("expected `sort` command, found {slice}")),
                    },
                    "function" => match list.as_slice() {
                        [_, Sexp::Atom(f), Sexp::List(_, xs), y, options @ ..] => {
                            let f = f.as_str().to_owned();
                            let xs = xs.iter().map(Sexp::to_type).collect::<Result<_, _>>()?;
                            let y = y.to_type()?;
                            let mut merge = None;
                            for option in options.chunks(2) {
                                let key = match &option[0] {
                                    Sexp::Atom(slice) => slice,
                                    Sexp::List(slice, _) => {
                                        return Err(format!("unknown option {slice}"))
                                    }
                                };
                                let value =
                                    option.get(1).ok_or(format!("missing value for {slice}"))?;
                                match key.as_str() {
                                    ":merge" => merge = Some(value.to_expr()?),
                                    _ => return Err(format!("unknown option {key}")),
                                }
                            }
                            Ok(vec![Command::Function(slice, f, xs, y, merge)])
                        }
                        _ => Err(format!("expected `function` command, found {slice}")),
                    },
                    "relation" => match list.as_slice() {
                        [_, Sexp::Atom(f), Sexp::List(_, xs)] => Ok(vec![Command::Function(
                            slice,
                            f.as_str().to_owned(),
                            xs.iter().map(Sexp::to_type).collect::<Result<_, _>>()?,
                            Type::Unit,
                            None,
                        )]),
                        _ => Err(format!("expected `relation` command, found {slice}")),
                    },
                    "datatype" => {
                        if let [_, Sexp::Atom(name), ..] = list.as_slice() {
                            let name = name.as_str().to_owned();
                            let mut out = vec![Command::Sort(slice, name.clone())];
                            for variant in list.drain(2..) {
                                match variant {
                                    Sexp::List(slice, list) => match list.as_slice() {
                                        [Sexp::Atom(variant), xs @ ..] => {
                                            out.push(Command::Function(
                                                slice,
                                                variant.as_str().to_owned(),
                                                xs.iter()
                                                    .map(Sexp::to_type)
                                                    .collect::<Result<_, _>>()?,
                                                Type::Sort(name.clone()),
                                                None,
                                            ));
                                        }
                                        _ => {
                                            return Err(format!(
                                                "expected `datatype` variant, found {slice}"
                                            ))
                                        }
                                    },
                                    Sexp::Atom(slice) => {
                                        return Err(format!(
                                            "expected `datatype` variant, found {slice}"
                                        ))
                                    }
                                }
                            }
                            Ok(out)
                        } else {
                            Err(format!("expected `datatype` command, found {slice}"))
                        }
                    }
                    "define" => match list.as_slice() {
                        [_, Sexp::Atom(x), e] => Ok(vec![
                            Command::Function(
                                // manual clone
                                Slice {
                                    source: slice.source,
                                    range: slice.range.start..slice.range.end,
                                },
                                x.as_str().to_owned(),
                                Vec::new(),
                                e.to_expr()?.get_type(funcs)?,
                                None,
                            ),
                            Command::Action(Action::Insert(
                                slice,
                                x.as_str().to_owned(),
                                Vec::new(),
                                e.to_expr()?,
                            )),
                        ]),
                        _ => Err(format!("expected `define` command, found {slice}")),
                    },
                    "rule" => match list.as_slice() {
                        [_, Sexp::List(_, patterns), Sexp::List(..)] => {
                            let patterns: Vec<_> = patterns
                                .iter()
                                .map(Sexp::to_pattern)
                                .collect::<Result<Vec<_>, _>>()?;
                            let actions: Vec<_> = match list.remove(2) {
                                Sexp::List(_, actions) => actions
                                    .into_iter()
                                    .map(|action| action.to_action(funcs))
                                    .collect::<Result<_, _>>()?,
                                Sexp::Atom(_) => unreachable!(),
                            };
                            Ok(vec![Command::Rule(slice, patterns, actions)])
                        }
                        _ => Err(format!("expeted `rule` command, found {slice}")),
                    },
                    "rewrite" => match list.as_slice() {
                        [_, a, b] => {
                            // This is slightly hacky, but we use a variable name with
                            // a space in it to make sure there are never any conflicts.
                            let x = Expr::Var(String::from(" _"));
                            let (a, b) = (a.to_expr()?, b.to_expr()?);
                            let ((Ok(Type::Sort(s)), _) | (_, Ok(Type::Sort(s)))) =
                                (a.get_type(funcs), b.get_type(funcs))
                            else {
                                return Err(format!("expected matching sorts, found {slice}"));
                            };
                            Ok(vec![Command::Rule(
                                slice,
                                vec![Pattern(vec![a, x.clone()])],
                                vec![Action::Union(b, x, s)],
                            )])
                        }
                        _ => Err(format!("expeted `rewrite` command, found {slice}")),
                    },
                    "run" => match list.as_slice() {
                        [_] => Ok(vec![Command::Run(slice)]),
                        _ => Err(format!("expeted `run` command, found {slice}")),
                    },
                    "check" => match list.as_slice() {
                        [_, patterns @ ..] => Ok(vec![Command::Check(
                            slice,
                            patterns
                                .iter()
                                .map(Sexp::to_pattern)
                                .collect::<Result<Vec<_>, _>>()?,
                        )]),
                        _ => Err(format!("expeted `check` command, found {slice}")),
                    },
                    _ => Ok(vec![Command::Action(
                        Sexp::List(slice, list).to_action(funcs)?,
                    )]),
                },
                _ => Err(format!("expected command, found {slice}")),
            },
            Sexp::Atom(slice) => Err(format!("expected command, found {slice}")),
        }
    }
}

/// Parse a source string into an `egglog` program.
#[allow(clippy::range_plus_one)]
pub fn parse(source: &Source) -> Result<Vec<Command>, String> {
    // split source text into slices
    let mut slices: Vec<(Slice, bool)> = Vec::new();
    let mut in_line_comment = false;
    let mut in_string = false;
    let mut in_escape = false;
    for (i, c) in source.text.char_indices() {
        let slice = Slice {
            source,
            range: i..i + 1,
        };
        match c {
            c if in_string => {
                slices.last_mut().unwrap().0.range.end += 1;
                if in_escape {
                    in_escape = false;
                } else if c == '\\' {
                    in_escape = true;
                } else if c == '"' {
                    in_string = false;
                }
            }

            '\n' if in_line_comment => in_line_comment = false,
            _ if in_line_comment => {}
            ';' => in_line_comment = true,

            '"' => {
                in_string = true;
                slices.push((slice, false));
            }
            '(' | ')' => slices.push((slice, false)),
            c if c.is_whitespace() => {}
            _ => match slices.last_mut() {
                Some((slice, is_atom)) if *is_atom && slice.range.end == i => {
                    slice.range.end += 1;
                }
                _ => slices.push((slice, true)),
            },
        }
    }

    if in_string {
        return Err(format!("unclosed string {}", slices.last_mut().unwrap().0));
    }

    // parse slice stream into sexps
    let mut sexps = Vec::new();
    let mut slices = slices.into_iter().map(|(slice, _)| slice).peekable();
    while slices.peek().is_some() {
        sexps.push(Sexp::parse(&mut slices)?);
    }

    // translate sexps into commands
    let mut commands = Vec::new();
    let mut funcs = HashMap::new();
    for sexp in sexps {
        commands.extend(sexp.to_commands(&funcs)?.into_iter().inspect(|command| {
            if let Command::Function(_, f, _, y, _) = command {
                funcs.insert(f.clone(), y.clone());
            }
        }));
    }
    Ok(commands)
}

impl Expr {
    /// Gets the type of this expression. `funcs` is a map to the return type.
    fn get_type(&self, funcs: &HashMap<String, Type>) -> Result<Type, String> {
        match self {
            Expr::Unit => Ok(Type::Unit),
            Expr::Int(_) => Ok(Type::Int),
            Expr::String(_) => Ok(Type::String),
            Expr::Var(_) => Err(format!("unknown type for {self}")),
            Expr::Call(f, _) => match f.as_str() {
                "+" | "*" | "min" => Ok(Type::Int),
                _ => Ok(funcs.get(f).ok_or(format!("unknown function {f}"))?.clone()),
            },
        }
    }
}

fn parse_to_string(slice: &Slice) -> Result<Option<String>, String> {
    let mut chars = slice.as_str().chars();
    if chars.next().unwrap() != '"' {
        return Ok(None);
    }
    if chars.next_back().unwrap() != '"' {
        return Err(format!("missing end quote in {slice}"));
    }
    let mut string = String::new();
    let mut in_escape = false;
    for c in chars {
        match c {
            '\\' if in_escape => string.push('\\'),
            '"' if in_escape => string.push('"'),
            't' if in_escape => string.push('\t'),
            'n' if in_escape => string.push('\n'),
            c if in_escape => return Err(format!("unknown escape code \\{c} in {slice}")),

            '\\' => in_escape = true,
            '"' => panic!("unexpected quote in {slice}"),
            c => string.push(c),
        }
    }
    Ok(Some(string))
}
