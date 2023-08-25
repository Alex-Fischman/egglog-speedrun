#![deny(clippy::pedantic)]

/// The path to the egglog executable.
const EGGLOG: &str = "target/debug/egglog";
/// The path to the directory of examples.
const EXAMPLES: &str = "examples";

#[test]
fn test_examples() {
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
        let mut output = std::process::Command::new(EGGLOG)
            .arg(&example)
            .output()
            .expect("could not run egglog");
        let mut actual = output.stdout;
        actual.append(&mut output.stderr);
        let actual = String::from_utf8(actual).unwrap();

        let mut expected = example.clone();
        assert!(expected.set_extension("out"));
        let expected = std::fs::read_to_string(&expected)
            .unwrap_or_else(|_| panic!("could not read {}", expected.display()));

        assert_eq!(expected, actual);
    }
}
