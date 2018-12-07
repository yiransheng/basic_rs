use either::Either;
use int_hash::IntHashMap;

use super::error::CompileError;
use crate::ast::variable::Variable;
use crate::ast::*;
use crate::vm::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum ArrayType {
    List,
    Table,
}

pub struct ArrayDims<'a> {
    chunk: &'a mut Chunk,
    types: IntHashMap<Variable, ArrayType>,
}

impl<'a> ArrayDims<'a> {
    pub fn new(chunk: &'a mut Chunk) -> Self {
        ArrayDims {
            types: IntHashMap::default(),
            chunk,
        }
    }
}

impl<'a> Visitor<Result<(), CompileError>> for ArrayDims<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for s in &prog.statements {
            self.visit_statement(s)?;
        }
        for (var, ty) in self.types.iter() {
            match ty {
                ArrayType::List => {
                    self.chunk.write_opcode(OpCode::InitArray, 0);
                    self.chunk.add_inline_operand(*var, 0);
                }
                ArrayType::Table => {
                    self.chunk.write_opcode(OpCode::InitArray2d, 0);
                    self.chunk.add_inline_operand(*var, 0);
                }
            }
        }

        Ok(())
    }

    fn visit_let(&mut self, stmt: &LetStmt) -> Result<(), CompileError> {
        self.visit_lvalue(&stmt.var)
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        for v in &stmt.vars {
            self.visit_lvalue(v)?;
        }
        Ok(())
    }

    fn visit_data(&mut self, _stmt: &DataStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result<(), CompileError> {
        for part in &stmt.parts {
            match part {
                Printable::Expr(expr) => {
                    self.visit_expr(expr)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn visit_goto(&mut self, _stmt: &GotoStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_gosub(&mut self, _stmt: &GosubStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result<(), CompileError> {
        self.visit_expr(&stmt.lhs)?;
        self.visit_expr(&stmt.rhs)?;
        Ok(())
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result<(), CompileError> {
        self.visit_expr(&stmt.from)?;
        self.visit_expr(&stmt.to)?;
        if let Some(ref expr) = stmt.step {
            self.visit_expr(expr)?;
        }
        Ok(())
    }

    fn visit_next(&mut self, _stmt: &NextStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result<(), CompileError> {
        self.visit_expr(&stmt.expr)
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result<(), CompileError> {
        for dim in &stmt.dims {
            match dim {
                Either::Left(list) => {
                    self.visit_list(list)?;
                }
                Either::Right(table) => {
                    self.visit_table(table)?;
                }
            }
        }

        Ok(())
    }

    fn visit_rem(&mut self) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_end(&mut self) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_stop(&mut self) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_return(&mut self) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_variable(&mut self, _lval: &Variable) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_list(&mut self, list: &List) -> Result<(), CompileError> {
        let ty = self.types.get(&list.var);
        match ty {
            Some(ArrayType::Table) => Err(CompileError::TableUsedAsList),
            Some(ArrayType::List) => Ok(()),
            None => {
                self.types.insert(list.var, ArrayType::List);
                Ok(())
            }
        }
    }

    fn visit_table(&mut self, table: &Table) -> Result<(), CompileError> {
        let ty = self.types.get(&table.var);
        match ty {
            Some(ArrayType::List) => Err(CompileError::ListUsedAsTable),
            Some(ArrayType::Table) => Ok(()),
            None => {
                self.types.insert(table.var, ArrayType::Table);
                Ok(())
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expression) -> Result<(), CompileError> {
        match expr {
            Expression::Lit(_n) => Ok(()),
            Expression::Var(v) => self.visit_lvalue(v),
            Expression::Call(_func, arg) => self.visit_expr(arg),
            Expression::Neg(expr) => self.visit_expr(expr),
            Expression::Add(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                Ok(())
            }
            Expression::Sub(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                Ok(())
            }
            Expression::Mul(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                Ok(())
            }
            Expression::Div(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                Ok(())
            }
            Expression::Pow(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                Ok(())
            }
        }
    }
}
