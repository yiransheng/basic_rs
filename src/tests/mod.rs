use crate::compiler::compile;
use crate::parser::Parser;
use crate::scanner::Scanner;

fn test_correct_program(name: &str, prog_and_output: &str) {
    println!("Running test for: {}", name);

    let mut parts = prog_and_output.split("-- out --");
    let prog = parts.next().expect("Test file should starts with program");
    let output = parts
        .next()
        .expect("Test file should include program output");

    let scanner = Scanner::new(prog);
    let ast = Parser::new(scanner).parse().unwrap();

    let mut vm = compile(&ast).expect("it should compile successfuly");

    let mut printed = Vec::new();
    vm.run(&mut printed).expect("no runtime error");

    let printed = ::std::str::from_utf8(&printed).unwrap();

    // avoid accidental newlines in source file, may let through
    // subtle print bugs
    assert_eq!(printed.trim(), output.trim(), "{}", name);
}

macro_rules! test_bas {
    ( $($x:expr),* ) => {{
	$(
            test_correct_program($x, include_str!($x));
        )*
    }};
}

#[test]
fn test_sample_programs() {
    test_bas!(
        "bas/power.bas",
        "bas/lin_eq.bas",
        "bas/sales.bas",
        "bas/game_of_life.bas"
    );
}
