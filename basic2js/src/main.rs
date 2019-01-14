mod codegen;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

use basic_rs::{compile, print_source_error, Parser, Scanner};
use structopt::StructOpt;

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
    fn js_path(&self) -> PathBuf {
        let mut path = self.out_dir.clone();
        path.push("index.js");
        path
    }
    fn js_lib_path(&self) -> PathBuf {
        let mut path = self.out_dir.clone();
        path.push("lib.js");
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

static JS: &'static str = include_str!("web/lib.js");
static HTML: &'static str = include_str!("web/index.html");

fn main() {
    let opt = Opt::from_args();
    let source = match read_source(&opt) {
        Ok(s) => s,
        Err(err) => return eprintln!("{}", err),
    };

    let scanner = Scanner::new(&source);
    let ast = Parser::new(scanner).parse().unwrap_or_else(|e| {
        print_source_error(&e, &source);
        ::std::process::exit(1)
    });

    let mut ir = compile(&ast).unwrap_or_else(|e| {
        print_source_error(&e, &source);
        ::std::process::exit(1)
    });

    ir.optimize();

    if opt.print {
        println!("{}", ir);
    }

    if !opt.dry_run {
        let mut buffer = File::create(opt.js_path()).unwrap();
        codegen::generate_js(&ir, &mut buffer);

        let mut buffer = File::create(opt.js_lib_path()).unwrap();
        buffer.write(JS.as_bytes()).expect("failed to write");

        let mut buffer = File::create(opt.html_path()).unwrap();
        buffer.write(HTML.as_bytes()).expect("failed to write");
    }
}
