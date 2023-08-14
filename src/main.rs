//! The command line interface for egglog.

#![deny(clippy::pedantic)]
#![deny(missing_docs)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

pub mod expr;
pub mod syntax;
pub mod table;

pub use crate::{expr::*, syntax::*, table::*};
pub use std::collections::HashMap;
pub use std::fmt::{Display, Formatter, Result as FmtResult};

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => eprintln!("{e}"),
    }
}

fn run() -> Result<(), String> {
    let args = std::env::args().collect::<Vec<String>>();
    let name = args.get(1).ok_or("usage: pass an egglog file")?.clone();
    let text = std::fs::read_to_string(&name).map_err(|_| format!("could not read {name}"))?;
    let source = std::rc::Rc::new(Source { name, text });

    let commands = parse(&source)?;
    let mut state = State::new();
    for command in commands {
        state.run_command(command)?;
    }
    Ok(())
}

struct State<'a> {
    sorts: Vec<String>,
    funcs: HashMap<String, Table>,
    rules: Vec<(Vec<Pattern>, Vec<Action<'a>>)>,
}

impl<'a> State<'a> {
    fn new() -> State<'a> {
        State {
            sorts: Vec::new(),
            funcs: HashMap::new(),
            rules: Vec::new(),
        }
    }

    fn run_command(&mut self, command: Command<'a>) -> Result<(), String> {
        let vars = HashMap::new();
        match command {
            Command::Sort(token, sort) => {
                if self.sorts.contains(&sort) {
                    return Err(format!("{sort} was declared before {token}"));
                }
                self.sorts.push(sort);
            }
            Command::Function(token, f, xs, y, merge) => {
                if self.funcs.contains_key(&f) {
                    return Err(format!("{f} was declared before {token}"));
                }
                self.funcs.insert(f, Table::new(xs, y, merge));
            }
            Command::Rule(_, patterns, actions) => self.rules.push((patterns, actions)),
            Command::Run(_) => self.run_fixpoint()?,
            Command::Check(token, expr) => {
                println!("{token}: {}", expr.evaluate(&vars, &self.funcs)?);
            }
            Command::Action(action) => {
                run_action(&action, &vars, &mut self.funcs)?;
            }
        }
        Ok(())
    }

    fn run_fixpoint(&mut self) -> Result<(), String> {
        let mut changed = true;
        while changed {
            changed = false;
            for (patterns, actions) in &self.rules {
                // per pattern, per row, list of assignments
                let binding: Vec<Vec<Vec<(&str, Value)>>> = patterns
                    .iter()
                    .map(|pattern| {
                        self.funcs[&pattern.f]
                            .rows()
                            .map(|(xs, _)| {
                                pattern
                                    .xs
                                    .iter()
                                    .zip(xs)
                                    .map(|(a, b)| (a.as_str(), *b))
                                    .collect()
                            })
                            .collect()
                    })
                    .collect();
                // per rows, per pattern, assignments
                let bindings = multi_cartesian_product(binding);
                // flatten, filter out non-matching assignments, convert to hashmaps
                let bindings = bindings.into_iter().filter_map(|binding| {
                    let mut out: HashMap<&str, Value> = HashMap::new();
                    for assignments in binding {
                        for (key, value) in assignments {
                            if let Some(v) = out.get(key) {
                                if *v != value {
                                    return None;
                                }
                            } else {
                                out.insert(key, value);
                            }
                        }
                    }
                    Some(out)
                });

                for binding in bindings {
                    for action in actions {
                        if run_action(action, &binding, &mut self.funcs)? {
                            changed = true;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

fn run_action(
    action: &Action,
    vars: &HashMap<&str, Value>,
    funcs: &mut HashMap<String, Table>,
) -> Result<bool, String> {
    match action {
        Action::Insert(_, f, xs, y) => {
            let xs = xs
                .iter()
                .map(|x| x.evaluate(vars, funcs))
                .collect::<Result<Vec<_>, _>>()?;
            let y = y.evaluate(vars, funcs)?;
            let changed = funcs
                .get_mut(f.as_str())
                .ok_or(format!("unknown function {f}"))?
                .insert(&xs, y)?;
            Ok(changed)
        }
    }
}

fn multi_cartesian_product<T: Clone>(vecs: Vec<Vec<T>>) -> Vec<Vec<T>> {
    vecs.into_iter().fold(vec![vec![]], |xss, ys| {
        let mut out = Vec::new();
        for xs in xss {
            for y in &ys {
                let mut xs = xs.clone();
                xs.push(y.clone());
                out.push(xs);
            }
        }
        out
    })
}
