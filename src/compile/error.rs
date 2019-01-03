use std::error::Error;
use std::fmt;

use super::control_flow_context::CfError;
use crate::ast::LineNo;

#[derive(Debug)]
pub enum CompileError {
    UnreachableCode(LineNo),
    Custom(&'static str),
    CfError(CfError),
}

impl From<CfError> for CompileError {
    fn from(err: CfError) -> Self {
        CompileError::CfError(err)
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let desc = self.description();
        write!(f, "{}", desc)
    }
}

impl Error for CompileError {
    fn description(&self) -> &str {
        match self {
            CompileError::Custom(s) => s,
            CompileError::UnreachableCode(_) => "Unreachable code",
            CompileError::CfError(_) => {
                "Control flow graph failed to initialize"
            }
        }
    }
}
