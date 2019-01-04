use std::error::Error;
use std::fmt;
use std::io;
use std::iter::repeat;
use unicode_width::UnicodeWidthStr;

use crate::codegen::WriteError;
use crate::compile::CompileError as CompileErrorInner;
use crate::parser::Error as ParseError;
use crate::scanner::SourceMapped;
use crate::vm::RuntimeError;

type CompileError = SourceMapped<CompileErrorInner>;

#[derive(Debug)]
pub enum InterpreterError {
    IoFail(io::Error),
    ParseFail(ParseError),
    CompileFail(CompileError),
    CodegenFail(WriteError),
    Runtime(RuntimeError),
}

impl From<io::Error> for InterpreterError {
    fn from(e: io::Error) -> Self {
        InterpreterError::IoFail(e)
    }
}
impl From<ParseError> for InterpreterError {
    fn from(e: ParseError) -> Self {
        InterpreterError::ParseFail(e)
    }
}
impl From<CompileError> for InterpreterError {
    fn from(e: CompileError) -> Self {
        InterpreterError::CompileFail(e)
    }
}
impl From<WriteError> for InterpreterError {
    fn from(e: WriteError) -> Self {
        InterpreterError::CodegenFail(e)
    }
}
impl From<RuntimeError> for InterpreterError {
    fn from(e: RuntimeError) -> Self {
        InterpreterError::Runtime(e)
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InterpreterError::IoFail(e) => {
                writeln!(formatter, "IO error")?;
                e.fmt(formatter)
            }
            InterpreterError::ParseFail(e) => {
                writeln!(formatter, "Parse error")?;
                e.fmt(formatter)
            }
            InterpreterError::CompileFail(e) => {
                writeln!(formatter, "Compile error")?;
                e.fmt(formatter)
            }
            InterpreterError::CodegenFail(e) => {
                writeln!(formatter, "Compile error")?;
                e.fmt(formatter)
            }
            InterpreterError::Runtime(e) => {
                writeln!(formatter, "Runtime error in Line: {}\n", e.line_no)?;
                e.error.fmt(formatter)
            }
        }
    }
}

impl Error for InterpreterError {
    fn description(&self) -> &str {
        match self {
            InterpreterError::IoFail(e) => e.description(),
            InterpreterError::ParseFail(e) => e.description(),
            InterpreterError::CompileFail(e) => e.description(),
            InterpreterError::CodegenFail(e) => e.description(),
            InterpreterError::Runtime(e) => e.error.description(),
        }
    }
}

pub fn print_source_error<E: Error>(err: SourceMapped<E>, source: &str) {
    let line = source.lines().enumerate().find_map(|(i, l)| {
        if i == err.loc.line {
            Some(l)
        } else {
            None
        }
    });
    eprintln!("{}", err.value);

    if let Some(line) = line {
        eprintln!("\n{}", line);
        let w = line
            .get(0..err.loc.col)
            .map(UnicodeWidthStr::width)
            .unwrap_or(0);
        let underline = repeat("-").take(w).collect::<String>();
        eprintln!("{}^", underline);
    }
}
