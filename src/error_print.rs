use std::error::Error;
use std::fmt;
use std::io;
use std::iter::repeat;
use unicode_width::UnicodeWidthStr;

use crate::compiler::CompileError;
use crate::parser::Error as ParseError;
use crate::scanner::SourceMapped;
use crate::vm::RuntimeError;

#[derive(Debug)]
pub enum InterpreterError {
    IoFail(io::Error),
    ParseFail(ParseError),
    CompileFail(CompileError),
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
impl From<RuntimeError> for InterpreterError {
    fn from(e: RuntimeError) -> Self {
        InterpreterError::Runtime(e)
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(std::error::Error::description(self))
    }
}

impl Error for InterpreterError {
    fn description(&self) -> &str {
        match self {
            InterpreterError::IoFail(e) => e.description(),
            InterpreterError::ParseFail(e) => e.description(),
            InterpreterError::CompileFail(_) => "Compile error",
            InterpreterError::Runtime(_) => "Runtime error",
        }
    }
}

pub fn print_source_error<E: Error>(err: SourceMapped<E>, source: &str) {
    let line = source.lines().enumerate().find_map(
        |(i, l)| {
            if i == err.loc.line {
                Some(l)
            } else {
                None
            }
        },
    );
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
