use super::error::CompileError;
use crate::ast::function::Func;
use crate::ast::variable::Variable;
use crate::ast::*;
use crate::vm::*;

pub struct FuncCompiler<'a> {
    function: Func,
    arg: Variable,
    chunk: &'a mut Chunk,
    line: usize,
}

impl<'a> FuncCompiler<'a> {
    pub fn new(function: Func, arg: Variable, line: usize, chunk: &'a mut Chunk) -> Self {
        FuncCompiler {
            function,
            arg,
            line,
            chunk,
        }
    }
}

impl<'a> Visitor<Result<(), CompileError>> for FuncCompiler<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_statement(&mut self, stmt: &Statement) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_let(&mut self, stmt: &LetStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_data(&mut self, stmt: &DataStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result<(), CompileError> {
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
            LValue::Variable(ref var) => self.visit_variable(var)?,
            _ => panic!(),
        }

        Ok(())
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

    fn visit_list(&mut self, list: &List) -> Result<(), CompileError> {
        Err(CompileError::IllegalFuncDef)
    }

    fn visit_table(&mut self, table: &Table) -> Result<(), CompileError> {
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
                self.visit_expr(expr);
                if func.is_native() {
                    self.chunk.write_opcode(OpCode::CallNative, self.line);
                } else {
                    self.chunk.write_opcode(OpCode::Call, self.line);
                }
                self.chunk.add_inline_operand(*func, self.line);
            }
            Expression::Neg(expr) => {
                self.visit_expr(expr);
                self.chunk.write_opcode(OpCode::Negate, self.line);
            }
            Expression::Add(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Add, self.line);
            }
            Expression::Sub(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Sub, self.line);
            }
            Expression::Mul(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Mul, self.line);
            }
            Expression::Div(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Div, self.line);
            }
            Expression::Pow(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Pow, self.line);
            }
        }

        Ok(())
    }
}
