#[cfg(test)]
#[macro_use]
extern crate quickcheck;

mod codegen;
mod compile;
mod error_print;
mod parser;
mod scanner;
mod vm;

pub mod ast;
pub mod ir;
pub mod relooper;

pub use crate::codegen::{codegen, WriteError};
pub use crate::compile::{compile, CompileError};
pub use crate::error_print::{print_source_error, InterpreterError};
pub use crate::parser::{Error as ParseError, Parser};
pub use crate::scanner::{Scanner, SourceLoc, SourceMapped};
pub use crate::vm::{RuntimeError, VM};
