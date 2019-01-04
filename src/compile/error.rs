use std::error::Error;
use std::fmt;

use super::control_flow_context::CfError;
use crate::ast::{Func, LineNo, Variable};

#[derive(Debug)]
pub enum CompileError {
    UnreachableCode(LineNo),
    ArrayDimentionError(Variable, &'static str),
    FunctionNotDefined(Func),
    ForWithoutNext(Variable),
    NextWithoutFor(Variable),
    EndInSubroutine,
    ReturnInMain,
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
        match self {
            CompileError::Custom(s) => write!(f, "{}", s),
            CompileError::ForWithoutNext(var) => {
                write!(f, "{}, Variable: {}", desc, var)
            }
            CompileError::NextWithoutFor(var) => {
                write!(f, "{}, Variable: {}", desc, var)
            }
            CompileError::EndInSubroutine => write!(f, "{}", desc),
            CompileError::ReturnInMain => write!(f, "{}", desc),
            CompileError::ArrayDimentionError(var, s) => {
                write!(f, "{}, {} Array: {}", desc, s, var)
            }
            CompileError::FunctionNotDefined(func) => {
                write!(f, "{}: {}", desc, func)
            }
            CompileError::UnreachableCode(line) => {
                write!(f, "Unreachable code at Line: {}", line)
            }
            CompileError::CfError(_) => write!(f, "{}", desc),
        }
    }
}

impl Error for CompileError {
    fn description(&self) -> &str {
        match self {
            CompileError::Custom(_) => "Compile error",
            CompileError::ForWithoutNext(_) => "FOR without NEXT",
            CompileError::NextWithoutFor(_) => "NEXT without FOR",
            CompileError::EndInSubroutine => {
                "Unexpected END/STOP in subroutine"
            }
            CompileError::ReturnInMain => {
                "Unexpected RETURN outside subroutine"
            }
            CompileError::ArrayDimentionError(..) => "Array dimension conflict",
            CompileError::FunctionNotDefined(_) => "Function not defined",
            CompileError::UnreachableCode(_) => "Unreachable code",
            CompileError::CfError(_) => {
                "Control flow graph failed to initialize"
            }
        }
    }
}
