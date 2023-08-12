#![deny(missing_docs)]

//! The command line interface for egglog.

pub mod parse;

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => eprintln!("{}", e),
    }
}

fn run() -> Result<(), String> {
    let args = std::env::args().collect::<Vec<String>>();
    let name = args.get(1).ok_or("usage: pass an egglog file")?.to_owned();
    let text = std::fs::read_to_string(&name).map_err(|_| format!("could not read {}", name))?;
    let source = std::rc::Rc::new(parse::Source { name, text });

    let parsed = parse::parse(&source)?;
    println!("parsed:");
    for sexp in &parsed {
        println!("{}", sexp);
    }
    println!();

    Ok(())
}
