use super::control_flow_context::CfError;

#[derive(Debug)]
pub enum CompileError {
    Custom(&'static str),
    CfError(CfError),
}

impl From<CfError> for CompileError {
    fn from(err: CfError) -> Self {
        CompileError::CfError(err)
    }
}
