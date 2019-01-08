use crate::ast::{Visitor as AstVisitor, *};
use crate::ir::Offset;
use either::Either;

use super::compiler::{Compiler, Pass};
use super::error::CompileError;

pub enum GlobalDefPass {}

impl Pass for GlobalDefPass {
    type State = Option<Variable>;
}

impl<'a> AstVisitor<Result<(), CompileError>> for Compiler<'a, GlobalDefPass> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for (i, stmt) in prog.statements.iter().enumerate() {
            self.line_index = i;
            self.visit_statement(stmt)?;
        }

        Ok(())
    }
    fn visit_let(&mut self, stmt: &LetStmt) -> Result<(), CompileError> {
        self.visit_lvalue(&stmt.var)?;
        self.visit_expr(&stmt.expr)
    }

    fn visit_input(&mut self, stmt: &InputStmt) -> Result<(), CompileError> {
        for var in &stmt.vars {
            self.visit_lvalue(var)?;
        }

        Ok(())
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        for var in &stmt.vars {
            self.visit_lvalue(var)?;
        }

        Ok(())
    }

    fn visit_data(&mut self, _stmt: &DataStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result<(), CompileError> {
        for part in &stmt.parts {
            if let Printable::Expr(expr) = part {
                self.visit_expr(expr)?;
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
        self.visit_expr(&stmt.rhs)
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result<(), CompileError> {
        self.visit_variable(&stmt.var)?;
        self.visit_expr(&stmt.from)?;
        self.visit_expr(&stmt.to)?;
        if let Some(ref step) = stmt.step {
            self.visit_expr(step)?;
        }

        Ok(())
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result<(), CompileError> {
        self.visit_variable(&stmt.var)
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result<(), CompileError> {
        self.state = Some(stmt.var);

        let r = self.visit_expr(&stmt.expr).map(|_| {
            self.builder.define_function(stmt.func);
        });

        self.state = None;
        r
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result<(), CompileError> {
        for dim in &stmt.dims {
            match dim {
                Either::Left(List { var, .. }) => {
                    self.builder.define_array(*var, Offset::OneD(())).map_err(
                        |_| {
                            CompileError::ArrayDimentionError(
                                *var,
                                "Table as list",
                            )
                        },
                    )?;
                }
                Either::Right(Table { var, .. }) => {
                    self.builder
                        .define_array(*var, Offset::TwoD((), ()))
                        .map_err(|_| {
                            CompileError::ArrayDimentionError(
                                *var,
                                "Table as list",
                            )
                        })?;
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

    fn visit_variable(&mut self, lval: &Variable) -> Result<(), CompileError> {
        if Some(*lval) != self.state {
            self.builder.define_global(*lval);
        }

        Ok(())
    }

    fn visit_list(&mut self, list: &List) -> Result<(), CompileError> {
        self.builder
            .define_array(list.var, Offset::OneD(()))
            .map_err(|_| {
                CompileError::ArrayDimentionError(list.var, "Table as list")
            })
    }

    fn visit_table(&mut self, table: &Table) -> Result<(), CompileError> {
        self.builder
            .define_array(table.var, Offset::TwoD((), ()))
            .map_err(|_| {
                CompileError::ArrayDimentionError(table.var, "List as table")
            })
    }

    fn visit_expr(&mut self, expr: &Expression) -> Result<(), CompileError> {
        match expr {
            Expression::Lit(_) => Ok(()),
            Expression::Var(v) => self.visit_lvalue(v),
            Expression::Neg(rhs) => self.visit_expr(rhs),
            Expression::Add(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)
            }
            Expression::Sub(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)
            }
            Expression::Mul(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)
            }
            Expression::Div(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)
            }
            Expression::Pow(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)
            }
            Expression::Call(func, rhs) => {
                self.visit_expr(rhs)?;
                if !func.is_native() {
                    self.builder.define_function(*func);
                }
                Ok(())
            }
        }
    }
}
