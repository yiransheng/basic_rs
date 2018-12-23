mod module;

use crate::module::WasmModule;
use basic_rs::{CompileError, Compiler, Parser, Scanner, Target};

fn main() {
    let source = include_str!("simple.bas");
    let scanner = Scanner::new(source);
    let ast = Parser::new(scanner).parse().unwrap();

    let compiler: Compiler<Target<WasmModule>> = Compiler::new();
    let (main_id, mut modules) = compiler.compile(&ast).unwrap();

    let wasm = modules.remove(&main_id).unwrap();

    wasm.optimize();

    // Ok(VM::new(main_id, chunks))
    wasm.print();
}
