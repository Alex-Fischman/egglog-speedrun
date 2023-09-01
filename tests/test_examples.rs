#![deny(clippy::pedantic)]

/// The path to the egglog executable.
const EGGLOG: &str = "target/debug/egglog";
/// The path to the directory of examples.
const EXAMPLES: &str = "examples";

#[test]
fn test_examples() {
    let mut errors = 0;

    for example in std::fs::read_dir(std::path::Path::new(EXAMPLES))
        .unwrap()
        .map(Result::unwrap)
        .inspect(|entry| {
            assert!(
                entry.file_type().unwrap().is_file(),
                "no directories are allowed in {EXAMPLES}"
            );
        })
        .map(|entry| entry.path())
        .filter(|path| match path.extension() {
            Some(extension) if extension == "egg" => true,
            Some(extension) if extension == "out" => false,
            _ => panic!("unexpected file {}", path.display()),
        })
    {
        let output = std::process::Command::new(EGGLOG)
            .arg(&example)
            .output()
            .expect("could not run egglog");

        if !output.stdout.is_empty() || !output.stderr.is_empty() {
            errors += 1;
            eprintln!("when running {}", example.display());
            if !output.stdout.is_empty() {
                eprintln!("{}", String::from_utf8(output.stdout).unwrap());
            }
            if !output.stderr.is_empty() {
                eprintln!("{}", String::from_utf8(output.stderr).unwrap());
            }
        }
    }

    match errors {
        0 => {}
        1 => panic!("1 error"),
        e => panic!("{e} errors"),
    }
}
