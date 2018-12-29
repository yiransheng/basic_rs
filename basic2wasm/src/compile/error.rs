#[derive(Debug)]
pub enum CompileError {
    Custom(&'static str),
}
