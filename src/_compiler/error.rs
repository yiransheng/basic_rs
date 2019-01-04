use std::error::Error;
use std::fmt;

#[derive(Debug, Clone)]
pub enum CompileError {
    Custom(&'static str),
    NextWithoutFor,
    CannotAssignTo(String),
    ListUsedAsTable,
    TableUsedAsList,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let desc = self.description();
        match self {
            CompileError::CannotAssignTo(s) => write!(f, "{}: {}", desc, s),
            _ => write!(f, "{}", desc),
        }
    }
}

impl Error for CompileError {
    fn description(&self) -> &str {
        match self {
            CompileError::Custom(s) => s,
            CompileError::NextWithoutFor => "NEXT without FOR",
            CompileError::CannotAssignTo(_) => "Cannot assign to Expression",
            CompileError::ListUsedAsTable => "List used as Table",
            CompileError::TableUsedAsList => "Table used as List",
        }
    }
}