mod compile;
// mod error_print;
mod codegen;
mod parser;
mod scanner;
mod vm;

pub mod ast;
pub mod ir;

pub use crate::compile::{compile, CompileError};
// pub use crate::error_print::InterpreterError;
pub use crate::codegen::{codegen, WriteError};
pub use crate::parser::{Error as ParseError, Parser};
pub use crate::scanner::{Scanner, SourceLoc, SourceMapped};
pub use crate::vm::{RuntimeError, VM};
