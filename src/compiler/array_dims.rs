use either::Either;
use int_hash::IntHashMap;

use super::error::CompileError;
use crate::ast::variable::Variable;
use crate::ast::*;
use crate::ir::{Instruction, InstructionKind, Visitor as IRVisitor};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum ArrayType {
    List(LineNo),
    Table(LineNo),
}

pub struct ArrayDims<V> {
    ir_visitor: V,
    types: IntHashMap<Variable, ArrayType>,
    globals: IntHashMap<Variable, LineNo>,
    local: Option<Variable>,
    line: LineNo,
}

impl<V: IRVisitor> ArrayDims<V> {
    pub fn new(ir_visitor: V) -> Self {
        ArrayDims {
            types: IntHashMap::default(),
            globals: IntHashMap::default(),
            local: None,
            ir_visitor,
            line: 0,
        }
    }
}

impl<V: IRVisitor> Visitor<Result<(), CompileError>> for ArrayDims<V>
where
    V::Error: Into<CompileError>,
{
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for s in &prog.statements {
            self.line = s.line_no;
            self.visit_statement(s)?;
        }
        for (var, line_no) in self.globals.iter() {
            let kind = InstructionKind::DefineGlobal(*var);
            self.ir_visitor
                .visit_instruction(Instruction {
                    label: None,
                    line_no: *line_no,
                    kind,
                })
                .map_err(V::Error::into)?;
        }
        for (var, ty) in self.types.iter() {
            match ty {
                ArrayType::List(line_no) => {
                    let kind = InstructionKind::InitArray(*var);

                    self.ir_visitor
                        .visit_instruction(Instruction {
                            label: None,
                            line_no: *line_no,
                            kind,
                        })
                        .map_err(V::Error::into)?;
                }
                ArrayType::Table(line_no) => {
                    let kind = InstructionKind::InitArray2d(*var);

                    self.ir_visitor
                        .visit_instruction(Instruction {
                            label: None,
                            line_no: *line_no,
                            kind,
                        })
                        .map_err(V::Error::into)?;
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
        self.visit_variable(&stmt.var)?;
        self.visit_expr(&stmt.from)?;
        self.visit_expr(&stmt.to)?;
        if let Some(ref expr) = stmt.step {
            self.visit_expr(expr)?;
        }
        Ok(())
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result<(), CompileError> {
        self.visit_variable(&stmt.var)
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result<(), CompileError> {
        self.local = Some(stmt.var);
        self.visit_expr(&stmt.expr)?;
        self.local = None;
        Ok(())
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

    fn visit_variable(&mut self, var: &Variable) -> Result<(), CompileError> {
        match self.local {
            Some(local_var) if local_var.eq(var) => Ok(()),
            _ => {
                if !self.globals.contains_key(var) {
                    self.globals.insert(*var, self.line);
                }
                Ok(())
            }
        }
    }

    fn visit_list(&mut self, list: &List) -> Result<(), CompileError> {
        let ty = self.types.get(&list.var);
        match ty {
            Some(ArrayType::Table(_)) => Err(CompileError::TableUsedAsList),
            Some(ArrayType::List(_)) => Ok(()),
            None => {
                self.types.insert(list.var, ArrayType::List(self.line));
                Ok(())
            }
        }
    }

    fn visit_table(&mut self, table: &Table) -> Result<(), CompileError> {
        let ty = self.types.get(&table.var);
        match ty {
            Some(ArrayType::List(_)) => Err(CompileError::ListUsedAsTable),
            Some(ArrayType::Table(_)) => Ok(()),
            None => {
                self.types.insert(table.var, ArrayType::Table(self.line));
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
