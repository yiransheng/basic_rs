use basic_rs::ast::{Visitor as AstVisitor, *};

use super::error::CompileError;
use crate::ir::{BinaryOp, Expr, LValue as LV, Offset, UnaryOp};

pub struct ExprCompiler {
    local: Option<Variable>,
}

impl ExprCompiler {
    pub fn new() -> Self {
        ExprCompiler { local: None }
    }

    pub fn lvalue(&mut self, lval: &LValue) -> Result<LV, CompileError> {
        match self.visit_lvalue(lval) {
            Ok(Expr::Get(lv)) => Ok(*lv),
            Err(err) => Err(err),
            _ => unreachable!(),
        }
    }
}

impl AstVisitor<Result<Expr, CompileError>> for ExprCompiler {
    fn visit_def(&mut self, stmt: &DefStmt) -> Result<Expr, CompileError> {
        self.local = Some(stmt.var);

        let expr = self.visit_expr(&stmt.expr);

        self.local = None;

        expr
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result<Expr, CompileError> {
        match stmt.op {
            Relop::Less => Ok(Expr::Binary(
                BinaryOp::Less,
                Box::new(self.visit_expr(&stmt.lhs)?),
                Box::new(self.visit_expr(&stmt.rhs)?),
            )),
            Relop::LessEqual => Ok(Expr::Unary(
                UnaryOp::Not,
                Box::new(Expr::Binary(
                    BinaryOp::Greater,
                    Box::new(self.visit_expr(&stmt.lhs)?),
                    Box::new(self.visit_expr(&stmt.rhs)?),
                )),
            )),
            Relop::Greater => Ok(Expr::Binary(
                BinaryOp::Greater,
                Box::new(self.visit_expr(&stmt.lhs)?),
                Box::new(self.visit_expr(&stmt.rhs)?),
            )),
            Relop::GreaterEqual => Ok(Expr::Unary(
                UnaryOp::Not,
                Box::new(Expr::Binary(
                    BinaryOp::Less,
                    Box::new(self.visit_expr(&stmt.lhs)?),
                    Box::new(self.visit_expr(&stmt.rhs)?),
                )),
            )),
            Relop::Equal => Ok(Expr::Binary(
                BinaryOp::Equal,
                Box::new(self.visit_expr(&stmt.lhs)?),
                Box::new(self.visit_expr(&stmt.rhs)?),
            )),
            Relop::NotEqual => Ok(Expr::Unary(
                UnaryOp::Not,
                Box::new(Expr::Binary(
                    BinaryOp::Equal,
                    Box::new(self.visit_expr(&stmt.lhs)?),
                    Box::new(self.visit_expr(&stmt.rhs)?),
                )),
            )),
        }
    }

    fn visit_variable(&mut self, var: &Variable) -> Result<Expr, CompileError> {
        let lval = match self.local {
            Some(v) if v.eq(var) => LV::Local(0),
            _ => LV::Global(*var),
        };

        Ok(Expr::Get(Box::new(lval)))
    }

    fn visit_list(&mut self, list: &List) -> Result<Expr, CompileError> {
        let index = self.visit_expr(&list.subscript)?;
        let offset = Offset::OneD(index);
        let lval = LV::ArrPtr(list.var, offset);

        Ok(Expr::Get(Box::new(lval)))
    }

    fn visit_table(&mut self, table: &Table) -> Result<Expr, CompileError> {
        let row = self.visit_expr(&table.subscript.0)?;
        let col = self.visit_expr(&table.subscript.1)?;

        let offset = Offset::TwoD(row, col);
        let lval = LV::ArrPtr(table.var, offset);

        Ok(Expr::Get(Box::new(lval)))
    }

    fn visit_expr(&mut self, expr: &Expression) -> Result<Expr, CompileError> {
        match expr {
            Expression::Lit(n) => Ok(Expr::Const(*n)),
            Expression::Var(v) => self.visit_lvalue(v),
            Expression::Neg(rhs) => {
                Ok(Expr::Unary(UnaryOp::Neg, Box::new(self.visit_expr(rhs)?)))
            }
            Expression::Add(lhs, rhs) => Ok(Expr::Binary(
                BinaryOp::Add,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Sub(lhs, rhs) => Ok(Expr::Binary(
                BinaryOp::Sub,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Mul(lhs, rhs) => Ok(Expr::Binary(
                BinaryOp::Mul,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Div(lhs, rhs) => Ok(Expr::Binary(
                BinaryOp::Div,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Pow(lhs, rhs) => Ok(Expr::Binary(
                BinaryOp::Pow,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            )),
            Expression::Call(func, rhs) => {
                let arg = Box::new(self.visit_expr(rhs)?);
                match func {
                    Func::Rnd => Ok(Expr::RandF64),
                    Func::Sin => Ok(Expr::Unary(UnaryOp::Sin, arg)),
                    Func::Cos => Ok(Expr::Unary(UnaryOp::Cos, arg)),
                    Func::Atn => Ok(Expr::Unary(UnaryOp::Atn, arg)),
                    Func::Exp => Ok(Expr::Unary(UnaryOp::Exp, arg)),
                    Func::Abs => Ok(Expr::Unary(UnaryOp::Abs, arg)),
                    Func::Log => Ok(Expr::Unary(UnaryOp::Log, arg)),
                    Func::Sqr => Ok(Expr::Unary(UnaryOp::Sqr, arg)),
                    Func::Int => Ok(Expr::Unary(UnaryOp::Trunc, arg)),
                    _ => {
                        let lval = LV::FnPtr(*func);
                        Ok(Expr::Call(Box::new(lval), arg))
                    }
                }
            }
        }
    }
}
