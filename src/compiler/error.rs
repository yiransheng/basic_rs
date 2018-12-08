use std::error::Error;
use std::fmt;

#[derive(Debug, Clone)]
pub enum CompileError {
    Custom(&'static str),
    DuplicatedLines(usize),
    LinesNotInOrder(usize, usize),
    NextWithoutFor,
    CannotAssignTo(String),
    IllegalFuncDef,
    ListUsedAsTable,
    TableUsedAsList,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let desc = self.description();
        match self {
            CompileError::LinesNotInOrder(a, b) => {
                write!(f, "{}: {} and {}", desc, a, b)
            }
            CompileError::DuplicatedLines(line) => {
                write!(f, "{}: {}", desc, line)
            }
            CompileError::CannotAssignTo(s) => write!(f, "{}: {}", desc, s),
            _ => write!(f, "{}", desc),
        }
    }
}

impl Error for CompileError {
    fn description(&self) -> &str {
        match self {
            CompileError::Custom(s) => s,
            CompileError::DuplicatedLines(_) => "Duplicated lines",
            CompileError::LinesNotInOrder(..) => "Lines not in order",
            CompileError::NextWithoutFor => "NEXT without FOR",
            CompileError::CannotAssignTo(_) => "Cannot assign to Expression",
            CompileError::IllegalFuncDef => "Illegal Function DEF",
            CompileError::ListUsedAsTable => "List used as Table",
            CompileError::TableUsedAsList => "Table used as List",
        }
    }
}
