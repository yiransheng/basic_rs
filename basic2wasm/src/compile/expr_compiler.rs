use basic_rs::ast::{Visitor as AstVisitor, *};

pub struct ExprCompiler<'a> {
    builder: &'a mut IRBuilder,
}

impl<'a> AstVisitor<Result<IRExpression, CompileError>> for ExprCompiler<'a> {}
