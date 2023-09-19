//! The command line interface for egglog.

#![deny(clippy::pedantic)]
#![deny(missing_docs)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::type_complexity)]
#![allow(clippy::similar_names)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::must_use_candidate)]

pub mod database;
pub mod expr;
pub mod query;
pub mod syntax;
pub mod table;
pub mod unionfind;

pub use crate::{database::*, expr::*, query::*, syntax::*, table::*, unionfind::*};
pub use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
pub use std::cmp::Ordering;
pub use std::collections::{BTreeMap, BTreeSet};
pub use std::env::args;
pub use std::fmt::{Display, Formatter, Result as FmtResult};
pub use std::fs::read_to_string;
pub use std::iter::{empty, once, Enumerate, FilterMap, Peekable};
pub use std::mem::{replace, size_of, take};
pub use std::ops::Range;
pub use std::time::Instant;
pub use std::vec::IntoIter;

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => eprintln!("{e}"),
    }
}

fn run() -> Result<(), String> {
    let args = args().collect::<Vec<String>>();
    let name = args.get(1).ok_or("usage: pass an egglog file")?.clone();
    let text = read_to_string(&name).map_err(|_| format!("could not read {name}"))?;
    let source = Source { name, text };
    let start = Instant::now();

    let commands = parse(&source)?;
    let mut database = Database::default();
    for command in commands {
        match command {
            Command::Sort(_, sort) => {
                database.sort(sort)?;
            }
            Command::Function(_, f, mut xs, y, merge) => {
                xs.push(y);
                database.function(f, xs, merge)?;
            }
            Command::Rule(slice, patterns, actions) => {
                database.rule(Query::new(slice, &database.funcs(), patterns)?, actions)?;
            }
            Command::Action(_, action) => {
                database.action(&action)?;
            }
            Command::Run(_, i) => database.run(i)?,
            Command::Check(slice, patterns) => {
                let string = format!("{slice}");
                if !database.check(&Query::new(slice, &database.funcs(), patterns)?)? {
                    println!("failure: {string}");
                    println!("{database}");
                }
            }
            Command::PrintSize(_, f) => {
                println!("Function {f} has size {}", database.get_table_len(&f)?);
            }
            Command::PrintStats(_) => {
                println!("Program has taken {:.3}s", start.elapsed().as_secs_f64());
            }
        }
    }
    Ok(())
}
