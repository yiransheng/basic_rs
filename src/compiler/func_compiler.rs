use super::error::CompileError;
use crate::ast::variable::Variable;
use crate::ast::*;
use crate::vm::*;

use crate::ir::{Instruction, InstructionKind, Visitor as IRVisitor};

pub struct FuncCompiler<V> {
    arg: Variable,
    ir_visitor: V,
    line: usize,
}

impl<V: Default> FuncCompiler<V> {
    pub fn new(arg: Variable, line: usize) -> Self {
        FuncCompiler {
            arg,
            line,
            ir_visitor: V::default(),
        }
    }
}

impl<V: IRVisitor> Visitor<Result<(), CompileError>> for FuncCompiler<V>
where
    V::Error: Into<CompileError>,
{
    fn visit_program(&mut self, _prog: &Program) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_statement(
        &mut self,
        _stmt: &Statement,
    ) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_let(&mut self, _stmt: &LetStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_read(&mut self, _stmt: &ReadStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_data(&mut self, _stmt: &DataStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_print(&mut self, _stmt: &PrintStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_goto(&mut self, _stmt: &GotoStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_gosub(&mut self, _stmt: &GosubStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_if(&mut self, _stmt: &IfStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_for(&mut self, _stmt: &ForStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_next(&mut self, _stmt: &NextStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_def(&mut self, _stmt: &DefStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_dim(&mut self, _stmt: &DimStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_rem(&mut self) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_end(&mut self) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_stop(&mut self) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_return(&mut self) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_lvalue(&mut self, lval: &LValue) -> Result<(), CompileError> {
        match lval {
            LValue::Variable(ref var) => self.visit_variable(var),
            _ => Err(CompileError::IllegalFuncDef),
        }
    }

    fn visit_variable(&mut self, lval: &Variable) -> Result<(), CompileError> {
        if *lval == self.arg {
            self.chunk.write_opcode(OpCode::GetLocal, self.line);
        } else {
            self.chunk.write_opcode(OpCode::GetGlobal, self.line);
            self.chunk.add_inline_operand(lval.clone(), self.line);
        }

        Ok(())
    }

    fn visit_list(&mut self, _list: &List) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_table(&mut self, _table: &Table) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_expr(&mut self, expr: &Expression) -> Result<(), CompileError> {
        match expr {
            Expression::Lit(n) => {
                self.chunk.write_opcode(OpCode::Constant, self.line);
                self.chunk.add_operand(*n, self.line);
            }
            Expression::Var(v) => {
                self.visit_lvalue(v)?;
            }
            Expression::Call(func, expr) => {
                self.visit_expr(expr)?;
                if func.is_native() {
                    self.chunk.write_opcode(OpCode::CallNative, self.line);
                    self.chunk.add_inline_operand(*func, self.line);
                } else {
                    self.chunk.write_opcode(OpCode::GetFunc, self.line);
                    self.chunk.add_inline_operand(*func, self.line);
                    self.chunk.write_opcode(OpCode::Call, self.line);
                }
            }
            Expression::Neg(expr) => {
                self.visit_expr(expr)?;
                self.chunk.write_opcode(OpCode::Negate, self.line);
            }
            Expression::Add(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Add, self.line);
            }
            Expression::Sub(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Sub, self.line);
            }
            Expression::Mul(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Mul, self.line);
            }
            Expression::Div(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Div, self.line);
            }
            Expression::Pow(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Pow, self.line);
            }
        }

        Ok(())
    }
}
