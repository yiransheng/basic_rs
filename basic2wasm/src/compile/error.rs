use super::control_flow_context::CfError;
use basic_rs::ast::LineNo;

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
