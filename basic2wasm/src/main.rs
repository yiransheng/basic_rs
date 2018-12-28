mod ir;

use crate::ir::compile;

use basic_rs::{Parser, Scanner};
use binaryen::{set_global_codegen_config, CodegenConfig};

fn main() {
    // set opt-level to -O3
    // optimization is important, as naive codegen prodcues
    // lots for locals (two for each for loop)
    // -O3 takes care of it nicely
    let conf = CodegenConfig {
        shrink_level: 0,
        optimization_level: 3,
    };
    set_global_codegen_config(&conf);

    let source = include_str!("simple.bas");
    let scanner = Scanner::new(source);
    let ast = Parser::new(scanner).parse().unwrap();

    let wasm = compile(&ast).unwrap();

    wasm.optimize();

    wasm.print();
}
