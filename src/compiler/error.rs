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
