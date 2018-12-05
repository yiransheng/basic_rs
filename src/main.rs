use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

use structopt::StructOpt;

mod ast;
mod compiler;
mod error_print;
mod parser;
mod scanner;
mod vm;

use crate::ast::Visitor;
use crate::compiler::Compiler;
use crate::error_print::{print_source_error, InterpreterError};
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::vm::{Chunk, VM};

#[derive(Debug, StructOpt)]
#[structopt(name = "basic_rs", about = "basic_rs file")]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

fn read_source(opt: Opt) -> Result<String, InterpreterError> {
    let mut file = File::open(opt.input)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    Ok(source)
}

fn run(source: &str) -> Result<(), InterpreterError> {
    let scanner = Scanner::new(source);
    let ast = Parser::new(scanner).parse()?;

    let mut chunk = Chunk::new();
    let mut compiler = Compiler::new(&mut chunk);

    compiler.visit_program(&ast)?;

    let mut vm = VM::new(chunk);
    let stdout = io::stdout();
    vm.run(stdout.lock())?;

    Ok(())
}

fn main() {
    let opt = Opt::from_args();
    let source = match read_source(opt) {
        Ok(s) => s,
        Err(err) => return eprintln!("{}", err),
    };

    match run(&source) {
        Ok(_) => {}
        Err(e) => match e {
            InterpreterError::ParseFail(e) => print_source_error(e, &source),
            _ => eprintln!("{:?}", e),
        },
    }
}
