//! The command line interface for egglog.

#![deny(clippy::pedantic)]
#![allow(clippy::range_plus_one)]
#![deny(missing_docs)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

pub mod syntax;
pub mod table;

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
    let source = std::rc::Rc::new(syntax::Source { name, text });

    let commands = syntax::parse(&source)?;
    for command in &commands {
        println!("{command}");
    }
    println!();

    Ok(())
}
