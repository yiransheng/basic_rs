#[macro_use]
extern crate keyword_token_derive;

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

use rand::rngs::SmallRng;
use rand::FromEntropy;
use structopt::StructOpt;

mod ast;
mod codegen;
mod compile;
mod error_print;
mod ir;
mod parser;
mod relooper;
mod scanner;
mod vm;

#[cfg(test)]
mod tests;

use crate::codegen::codegen;
use crate::compile::compile;
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

fn read_source(opt: &Opt) -> Result<String, io::Error> {
    let mut file = File::open(&opt.input)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    Ok(source)
}

fn run(source: &str, opt: &Opt) -> Result<(), InterpreterError> {
    let scanner = Scanner::new(source);
    let ast = Parser::new(scanner).parse()?;

    let mut ir = compile(&ast)?;
    ir.optimize();

    if opt.disassemble {
        println!("{}", ir);
    }

    let mut vm = codegen(&ir)?;

    let stdout = io::stdout();
    let stdin = io::stdin();

    if opt.disassemble {
        vm.disassemble(stdout.lock());
    }

    let mut rng = SmallRng::from_entropy();

    vm.run_with_input(stdin.lock(), stdout.lock(), &mut rng)?;

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
            InterpreterError::ParseFail(e) => print_source_error(&e, &source),
            InterpreterError::CompileFail(e) => print_source_error(&e, &source),
            _ => eprintln!("{}", e),
        },
    }
}
