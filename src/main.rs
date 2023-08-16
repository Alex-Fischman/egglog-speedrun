//! The command line interface for egglog.

#![deny(clippy::pedantic)]
#![deny(missing_docs)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

pub mod database;
pub mod expr;
pub mod query;
pub mod syntax;
pub mod table;
pub mod unionfind;

pub use crate::{database::*, expr::*, query::*, syntax::*, table::*, unionfind::*};
pub use std::collections::{HashMap, HashSet};
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
    let mut database = Database::default();
    for command in commands {
        match command {
            Command::Sort(_, sort) => {
                database.sort(sort)?;
            }
            Command::Function(_, f, xs, y, merge) => {
                database.function(f, xs, y, merge)?;
            }
            Command::Rule(_, patterns, actions) => {
                database.rule(Query::new(&patterns)?, actions)?;
            }
            Command::Action(action) => {
                database.action(&action)?;
            }
            Command::Run(_) => database.run()?,
            Command::Check(token, patterns) => {
                if database.check(&Query::new(&patterns)?) {
                    println!("failure: {token}");
                }
            }
        }
    }
    Ok(())
}
