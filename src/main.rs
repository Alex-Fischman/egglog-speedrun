mod parse;
mod token;

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
    let source = std::rc::Rc::new(token::Source { name, text });

    let tokens = token::tokenize(&source);
    println!("tokens:");
    for token in &tokens {
        println!("{}", token);
    }
    println!();

    let parsed = parse::parse(&tokens)?;
    println!("parsed:");
    for sexp in &parsed {
        println!("{}", sexp);
    }
    println!();

    Ok(())
}
