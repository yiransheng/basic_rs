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

    /// Output dir
    #[structopt(
        parse(from_os_str),
        short = "o",
        long = "out-dir",
        default_value = "."
    )]
    out_dir: PathBuf,

    #[structopt(short = "p", long = "print")]
    print: bool,

    #[structopt(short = "d", long = "dry-run")]
    dry_run: bool,
}

impl Opt {
    fn wasm_path(&self) -> PathBuf {
        let mut path = self.out_dir.clone();
        path.push("main.wasm");
        path
    }
    fn js_path(&self) -> PathBuf {
        let mut path = self.out_dir.clone();
        path.push("index.js");
        path
    }
    fn html_path(&self) -> PathBuf {
        let mut path = self.out_dir.clone();
        path.push("index.html");
        path
    }
}

fn read_source(opt: &Opt) -> Result<String, io::Error> {
    let mut file = File::open(&opt.input)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    Ok(source)
}

static JS: &'static str = include_str!("web/index.js");
static HTML: &'static str = include_str!("web/index.html");

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

    if opt.print {
        wasm.print();
    }

    if !opt.dry_run {
        let code = wasm.write();
        let mut buffer = File::create(opt.wasm_path()).unwrap();
        buffer.write(&code).expect("failed to write");

        let mut buffer = File::create(opt.js_path()).unwrap();
        buffer.write(JS.as_bytes()).expect("failed to write");

        let mut buffer = File::create(opt.html_path()).unwrap();
        buffer.write(HTML.as_bytes()).expect("failed to write");
    }
}
