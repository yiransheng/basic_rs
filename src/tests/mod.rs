use matches::*;
use rand::rngs::SmallRng;
use rand::SeedableRng;

use crate::codegen::codegen;
use crate::compile::compile;
use crate::parser::{ErrorInner, Parser};
use crate::scanner::{Error as ScannerError, Scanner, SourceLoc};

macro_rules! test_bas {
    ( $($x:expr),* ) => {{
	$(
            test_correct_program($x, include_str!($x));
        )*
    }};
}

fn test_correct_program(name: &str, prog_and_output: &str) {
    println!("Running test for: {}", name);

    let mut parts = prog_and_output.split("-- out --");
    let prog = parts.next().expect("Test file should starts with program");
    let output = parts
        .next()
        .expect("Test file should include program output");

    let scanner = Scanner::new(prog);
    let ast = Parser::new(scanner).parse().unwrap();

    let ir = compile(&ast).expect("it should compile successfuly");
    let mut vm = codegen(&ir).unwrap();

    let mut printed = Vec::new();
    let mut rng = SmallRng::from_seed([123; 16]);
    vm.run(&mut printed, &mut rng).expect("no runtime error");

    let printed = ::std::str::from_utf8(&printed).unwrap();

    // avoid accidental newlines in source file, may let through
    // subtle print bugs
    assert_eq!(printed.trim(), output.trim(), "{}", name);
}

fn parse_error(prog: &str) -> ErrorInner {
    let scanner = Scanner::new(prog);
    Parser::new(scanner).parse().unwrap_err().value
}

fn parse_error_source_loc(prog: &str) -> SourceLoc {
    let scanner = Scanner::new(prog);
    Parser::new(scanner).parse().unwrap_err().loc
}

#[test]
fn test_sample_programs() {
    test_bas!(
        "bas/let.bas",
        "bas/rem.bas",
        "bas/read.bas",
        "bas/const_if.bas",
        "bas/for.bas",
        "bas/bad_for.bas",
        "bas/def.bas",
        "bas/empty_print.bas",
        "bas/all_statements.bas",
        "bas/power.bas",
        "bas/lin_eq.bas",
        "bas/sales.bas",
        "bas/game_of_life.bas"
    );
}

#[test]
fn test_parse_error() {
    assert_matches!(
        parse_error(include_str!("parse_fail/bad_name.bas")),
        ErrorInner::ScanError(ScannerError::BadIdentifier(_))
    );
    assert_matches!(
        parse_error(include_str!("parse_fail/bad_number.bas")),
        ErrorInner::ScanError(ScannerError::BadNumber)
    );
    assert_matches!(
        parse_error(include_str!("parse_fail/generic_error.bas")),
        ErrorInner::UnexpectedToken(_)
    );
    assert_matches!(
        parse_error(include_str!("parse_fail/missing_line_no.bas")),
        ErrorInner::BadLineNo
    );
}

#[test]
fn test_parse_source_loc() {
    let loc =
        parse_error_source_loc(include_str!("parse_fail/fail_line_9.bas"));

    assert_eq!(loc.line, 9);
}
