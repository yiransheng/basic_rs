mod ir;

use crate::ir::compile;
use basic_rs::{CompileError, Compiler, Parser, Scanner, Target};

fn main() {
    let source = include_str!("simple.bas");
    let scanner = Scanner::new(source);
    let ast = Parser::new(scanner).parse().unwrap();

    let wasm = compile(&ast).unwrap();

    wasm.optimize();

    // Ok(VM::new(main_id, chunks))
    wasm.print();
}
