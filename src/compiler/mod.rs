mod anon_var;
mod array_dims;
mod compiler;
mod data;
mod error;
// mod func_compiler;
mod ir_labels;
mod line_order;

pub use self::compiler::{CompileError, Compiler, Target};

use crate::ast;
use crate::ir::Visitor;
use crate::vm::from_ir::{ChunkWriter, WriteError};
use crate::vm::VM;

impl Into<self::error::CompileError> for WriteError {
    fn into(self) -> self::error::CompileError {
        self::error::CompileError::Custom(
            "Compile error, failed to write byte code",
        )
    }
}

pub fn compile(ast: &ast::Program) -> Result<VM, CompileError> {
    let compiler: Compiler<Target<ChunkWriter>> = Compiler::new();
    let (main_id, chunks) = compiler.compile(ast)?;

    Ok(VM::new(main_id, chunks))
}
