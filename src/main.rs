use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

use structopt::StructOpt;

mod ast;
mod compiler;
mod error_print;
mod ir;
mod parser;
mod scanner;
mod vm;

#[cfg(test)]
mod tests;

use crate::compiler::compile;
use crate::error_print::{print_source_error, InterpreterError};
use crate::parser::Parser;
use crate::scanner::Scanner;

#[derive(Debug, StructOpt)]
#[structopt(name = "basic_rs", about = "basic_rs file")]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    #[structopt(short = "d", long = "disassemble")]
    disassemble: bool,
}

fn read_source(opt: &Opt) -> Result<String, InterpreterError> {
    let mut file = File::open(&opt.input)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    Ok(source)
}

fn run(source: &str, opt: &Opt) -> Result<(), InterpreterError> {
    let scanner = Scanner::new(source);
    let ast = Parser::new(scanner).parse()?;

    let mut vm = compile(&ast)?;

    let stdout = io::stdout();

    if opt.disassemble {
        vm.disassemble(stdout.lock());
    }

    vm.run(stdout.lock())?;

    Ok(())
}

fn main() {
    let opt = Opt::from_args();
    let source = match read_source(&opt) {
        Ok(s) => s,
        Err(err) => return eprintln!("{}", err),
    };

    match run(&source, &opt) {
        Ok(_) => {}
        Err(e) => match e {
            InterpreterError::ParseFail(e) => print_source_error(e, &source),
            _ => eprintln!("{}", e),
        },
    }
}
