#[derive(Debug, Copy, Clone)]
pub enum CompileError {
    Custom(&'static str),
    IllegalFuncDef,
}
