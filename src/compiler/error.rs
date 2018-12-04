#[derive(Debug, Clone)]
pub enum CompileError {
    Custom(&'static str),
    NextWithoutFor,
    CannotAssignTo(String),
    IllegalFuncDef,
    ListUsedAsTable,
    TableUsedAsList,
}
