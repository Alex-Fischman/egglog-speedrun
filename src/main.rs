//! The command line interface for egglog.

// Force clippy to be annoying, then remove bad lints.
#![deny(missing_docs)]
#![deny(clippy::pedantic)]
// RangeInclusive doesn't work for `Token`s.
#![allow(clippy::range_plus_one)]

pub mod syntax;

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
