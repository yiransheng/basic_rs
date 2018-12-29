use basic_rs::ast::{Visitor as AstVisitor, *};

use super::error::CompileError;
use crate::ir::Builder;

pub struct GlobalDefPass<'a> {
    builder: &'a mut Builder,
    local: Option<Variable>,
}

impl<'a> GlobalDefPass<'a> {
    pub fn new(builder: &'a mut Builder) -> Self {
        GlobalDefPass {
            builder,
            local: None,
        }
    }
}

impl<'a> AstVisitor<Result<(), CompileError>> for GlobalDefPass<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for stmt in &prog.statements {
            self.visit_statement(stmt)?;
        }

        Ok(())
    }
    fn visit_let(&mut self, stmt: &LetStmt) -> Result<(), CompileError> {
        self.visit_lvalue(&stmt.var)
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        for var in &stmt.vars {
            self.visit_lvalue(var)?;
        }

        Ok(())
    }

    fn visit_data(&mut self, stmt: &DataStmt) -> Result<(), CompileError> {
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

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result<(), CompileError> {
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
        self.local = Some(stmt.var);

        let r = self.visit_expr(&stmt.expr).map(|_| {
            self.builder.define_function(stmt.func);
        });

        self.local = None;
        r
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result<(), CompileError> {
        unimplemented!()
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
        if Some(*lval) != self.local {
            self.builder.define_global(*lval);
        }

        Ok(())
    }

    fn visit_list(&mut self, list: &List) -> Result<(), CompileError> {
        self.builder.define_array(list.var);

        Ok(())
    }

    fn visit_table(&mut self, table: &Table) -> Result<(), CompileError> {
        self.builder.define_array(table.var);

        Ok(())
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
