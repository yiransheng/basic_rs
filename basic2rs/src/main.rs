mod codegen;

use std::fs::{create_dir_all, File};
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

use basic_rs::{compile, print_source_error, Parser, Scanner};
use structopt::StructOpt;
use toml::{self, Value};

#[derive(Debug, StructOpt)]
#[structopt(name = "bas2rs", about = "bas2rs file")]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Output dir
    #[structopt(parse(from_os_str), short = "o", long = "out-dir")]
    out_dir: PathBuf,

    #[structopt(short = "p", long = "print")]
    print: bool,

    #[structopt(short = "d", long = "dry-run")]
    dry_run: bool,
}

impl Opt {
    fn app_name(&self) -> Option<&str> {
        self.input
            .as_path()
            .file_stem()
            .and_then(|name| name.to_str())
    }

    fn rs_path(&self) -> PathBuf {
        let mut path = self.out_dir.clone();
        path.push("src/main.rs");
        path
    }
    fn runtime_path(&self) -> PathBuf {
        let mut path = self.out_dir.clone();
        path.push("src/runtime.rs");
        path
    }
    fn conf_path(&self) -> PathBuf {
        let mut path = self.out_dir.clone();
        path.push("Cargo.toml");
        path
    }

    fn create_sub_dirs(&self) {
        let path = self.out_dir.as_path();
        assert!(!path.exists() || path.is_dir(), "Invalid output directory");

        let mut src = self.out_dir.clone();
        src.push("src");
        create_dir_all(src.as_path()).unwrap();
    }
}

fn read_source(opt: &Opt) -> Result<String, io::Error> {
    let mut file = File::open(&opt.input)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    Ok(source)
}

static RS_LIB: &'static str = include_str!("_lib.rs");
static CARGO_TOML: &'static str = include_str!("_Cargo.toml");

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
        let mut cargo_toml = CARGO_TOML.parse::<Value>().unwrap();
        cargo_toml["package"]["name"] =
            opt.app_name().map(Value::from).unwrap();
        opt.create_sub_dirs();

        let mut buffer = File::create(opt.conf_path()).unwrap();
        buffer
            .write(toml::to_string_pretty(&cargo_toml).unwrap().as_bytes())
            .expect("failed to write Cargo.toml");

        let mut buffer = File::create(opt.runtime_path()).unwrap();
        buffer
            .write(RS_LIB.as_bytes())
            .expect("failed to write runtime.rs");

        let mut buffer = File::create(opt.rs_path()).unwrap();
        codegen::generate_rs(&ir, &mut buffer);
    }
}
