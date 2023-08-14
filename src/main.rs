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
            Command::Run(_) => self.run_fixpoint(),
            Command::Check(token, expr) => {
                println!("{token}: {}", expr.evaluate(&vars, &self.funcs)?);
            }
            Command::Action(action) => {
                self.run_action(&action, &vars)?;
            }
        }
        Ok(())
    }

    fn run_fixpoint(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;
            for _rule in &self.rules {
                todo!("match pattern, bind vars, run actions")
            }
        }
    }

    fn run_action(&mut self, action: &Action, vars: &HashMap<&str, Value>) -> Result<bool, String> {
        match action {
            Action::Insert(_, f, xs, y) => {
                let xs = xs
                    .iter()
                    .map(|x| x.evaluate(vars, &self.funcs))
                    .collect::<Result<Vec<_>, _>>()?;
                let y = y.evaluate(vars, &self.funcs)?;
                let changed = self
                    .funcs
                    .get_mut(f.as_str())
                    .ok_or(format!("unknown function {f}"))?
                    .insert(&xs, y)?;
                Ok(changed)
            }
        }
    }
}
