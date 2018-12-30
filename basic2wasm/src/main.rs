mod compile;
mod ir;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

use basic_rs::{Parser, Scanner};
use binaryen::{set_global_codegen_config, CodegenConfig};
use structopt::StructOpt;

use crate::compile::compile;
use crate::ir::CodeGen;

#[derive(Debug, StructOpt)]
#[structopt(name = "bas2wasm", about = "bas2wasm file")]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

fn read_source(opt: &Opt) -> Result<String, io::Error> {
    let mut file = File::open(&opt.input)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    Ok(source)
}

fn main() {
    let opt = Opt::from_args();
    let source = match read_source(&opt) {
        Ok(s) => s,
        Err(err) => return eprintln!("{}", err),
    };

    let conf = CodegenConfig {
        shrink_level: 1,
        optimization_level: 3,
    };
    set_global_codegen_config(&conf);

    let scanner = Scanner::new(&source);
    let ast = Parser::new(scanner).parse().unwrap();

    let ir = compile(&ast).unwrap();
    let wasm = CodeGen::new(ir).generate();

    wasm.optimize();

    wasm.print();

    let mut buffer = File::create("main.wasm").unwrap();
    let code = wasm.write();

    buffer.write(&code).expect("failed to write");
}
