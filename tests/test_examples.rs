#![deny(clippy::pedantic)]

/// The path to the egglog executable.
const EGGLOG: &str = "target/debug/egglog";
/// The path to the directory of examples.
const EXAMPLES: &str = "examples";

#[test]
fn test_examples() {
    let _command = std::process::Command::new(EGGLOG);

    for example in std::fs::read_dir(std::path::Path::new(EXAMPLES))
        .unwrap()
        .map(Result::unwrap)
        .inspect(|entry| assert!(entry.file_type().unwrap().is_file()))
        .map(|entry| entry.path())
        .filter(|path| match path.extension() {
            Some(extension) if extension == "egg" => true,
            Some(extension) if extension == "out" => false,
            _ => panic!("expected either .egg or .out, found {}", path.display()),
        })
    {
        let mut expected = example.clone();
        assert!(expected.set_extension("out"));

        panic!("{example:?} {expected:?}");
    }
}
