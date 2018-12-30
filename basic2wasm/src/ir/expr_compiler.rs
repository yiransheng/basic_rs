use super::builder::IRBuilder;
use super::compiler::CompileError;
use super::{
    BinaryOp, Expression as IRExpression, JumpKind, Label,
    Statement as IRStatement, UnaryOp, IR,
};
use basic_rs::ast::{Visitor as AstVisitor, *};

pub struct ExprCompiler<'a> {
    builder: &'a mut IRBuilder,
}

impl<'a> From<&'a mut IRBuilder> for ExprCompiler<'a> {
    fn from(builder: &'a mut IRBuilder) -> Self {
        ExprCompiler { builder }
    }
}

impl<'a> AstVisitor<Result<IRExpression, CompileError>> for ExprCompiler<'a> {
    fn visit_program(
        &mut self,
        prog: &Program,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_let(
        &mut self,
        stmt: &LetStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_read(
        &mut self,
        stmt: &ReadStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_data(
        &mut self,
        stmt: &DataStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_print(
        &mut self,
        stmt: &PrintStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_goto(
        &mut self,
        stmt: &GotoStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_gosub(
        &mut self,
        stmt: &GosubStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_if(
        &mut self,
        stmt: &IfStmt,
    ) -> Result<IRExpression, CompileError> {
        match stmt.op {
            Relop::Less => Ok(IRExpression::Binary(
                BinaryOp::Less,
                Box::new(self.visit_expr(&stmt.lhs)?),
                Box::new(self.visit_expr(&stmt.rhs)?),
            )),
            Relop::LessEqual => Ok(IRExpression::Unary(
                UnaryOp::EqZ,
                Box::new(IRExpression::Binary(
                    BinaryOp::Greater,
                    Box::new(self.visit_expr(&stmt.lhs)?),
                    Box::new(self.visit_expr(&stmt.rhs)?),
                )),
            )),
            Relop::Greater => Ok(IRExpression::Binary(
                BinaryOp::Greater,
                Box::new(self.visit_expr(&stmt.lhs)?),
                Box::new(self.visit_expr(&stmt.rhs)?),
            )),
            Relop::GreaterEqual => Ok(IRExpression::Unary(
                UnaryOp::EqZ,
                Box::new(IRExpression::Binary(
                    BinaryOp::Less,
                    Box::new(self.visit_expr(&stmt.lhs)?),
                    Box::new(self.visit_expr(&stmt.rhs)?),
                )),
            )),
            Relop::Equal => Ok(IRExpression::Binary(
                BinaryOp::Equal,
                Box::new(self.visit_expr(&stmt.lhs)?),
                Box::new(self.visit_expr(&stmt.rhs)?),
            )),
            Relop::NotEqual => Ok(IRExpression::Unary(
                UnaryOp::EqZ,
                Box::new(IRExpression::Binary(
                    BinaryOp::Equal,
                    Box::new(self.visit_expr(&stmt.lhs)?),
                    Box::new(self.visit_expr(&stmt.rhs)?),
                )),
            )),
        }
    }

    fn visit_for(
        &mut self,
        stmt: &ForStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_next(
        &mut self,
        stmt: &NextStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_def(
        &mut self,
        stmt: &DefStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_dim(
        &mut self,
        stmt: &DimStmt,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_rem(&mut self) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_end(&mut self) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_stop(&mut self) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_return(&mut self) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_variable(
        &mut self,
        lval: &Variable,
    ) -> Result<IRExpression, CompileError> {
        let sym = self.builder.sym_global(*lval);
        Ok(IRExpression::Get(sym))
    }

    fn visit_list(
        &mut self,
        list: &List,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_table(
        &mut self,
        table: &Table,
    ) -> Result<IRExpression, CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_expr(
        &mut self,
        expr: &Expression,
    ) -> Result<IRExpression, CompileError> {
        match expr {
            Expression::Lit(n) => Ok(IRExpression::Const(*n)),
            Expression::Var(v) => self.visit_lvalue(v),
            Expression::Neg(rhs) => Ok(IRExpression::Unary(
                UnaryOp::Neg,
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Add(lhs, rhs) => Ok(IRExpression::Binary(
                BinaryOp::Add,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Sub(lhs, rhs) => Ok(IRExpression::Binary(
                BinaryOp::Sub,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Mul(lhs, rhs) => Ok(IRExpression::Binary(
                BinaryOp::Mul,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Div(lhs, rhs) => Ok(IRExpression::Binary(
                BinaryOp::Div,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            _ => Err(CompileError::Unsupported),
        }
    }
}
