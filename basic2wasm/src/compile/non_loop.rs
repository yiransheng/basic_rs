use basic_rs::ast::{Visitor as AstVisitor, *};
use slotmap::SecondaryMap;

use super::control_flow_context::CfCtx;
use super::error::CompileError;
use super::expr_compiler::ExprCompiler;
use crate::ir::{
    BasicBlock, Builder, Expr, Function, FunctionName, LValue as LV, Label,
    Statement as IRStatement,
};

pub struct NonLoopPass<'a> {
    cf_ctx: &'a CfCtx,
    builder: &'a mut Builder,

    line_index: usize,
    main: Option<FunctionName>,
}
impl<'a> NonLoopPass<'a> {
    pub fn new(cf_ctx: &'a CfCtx, builder: &'a mut Builder) -> Self {
        NonLoopPass {
            cf_ctx,
            builder,
            line_index: 0,
            main: None,
        }
    }

    fn add_statement(&mut self, stmt: IRStatement) -> Result<(), CompileError> {
        let current_func = self.cf_ctx.get_func(self.line_index).unwrap();
        let current_label = self.cf_ctx.get_label(self.line_index).unwrap();

        self.builder
            .add_statement(current_func, current_label, stmt)
            .map_err(|_| CompileError::Custom("function or block not found"))
    }

    fn add_basic_block_branch(&mut self) {
        let current_label = self.current_label();
        let next_label = self.next_line_label();

        if let Some(next_label) = next_label {
            if next_label != current_label {
                self.builder.add_branch(
                    self.current_func(),
                    current_label,
                    next_label,
                );
            }
        }
    }

    fn current_label(&self) -> Label {
        self.cf_ctx.get_label(self.line_index).unwrap()
    }

    fn current_func(&self) -> FunctionName {
        self.cf_ctx.get_func(self.line_index).unwrap()
    }

    fn next_line_label(&self) -> Option<Label> {
        let current_func = self.current_func();
        let next_func = self.cf_ctx.get_func(self.line_index + 1);
        if Some(current_func) == next_func {
            self.cf_ctx.get_label(self.line_index + 1)
        } else {
            None
        }
    }
}

impl<'a> AstVisitor<Result<(), CompileError>> for NonLoopPass<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for (func, entry) in self.cf_ctx.functions() {
            self.builder
                .add_function(func, entry)
                .map_err(|_| CompileError::Custom("function already exist"))?;
        }

        let main_func = self.cf_ctx.get_func(0).unwrap();
        self.main = Some(main_func);
        self.builder
            .set_main(main_func)
            .map_err(|_| CompileError::Custom("main already set"))?;

        for i in 0..prog.statements.len() {
            let func = self.cf_ctx.get_func(i);
            let label = self.cf_ctx.get_label(i);

            match (func, label) {
                (Some(func), Some(label)) => {
                    let _ = self.builder.add_block(func, label);
                }
                _ => {}
            }
        }

        for (i, stmt) in prog.statements.iter().enumerate() {
            self.line_index = i;
            self.visit_statement(stmt)?;
        }

        Ok(())
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        let mut expr_compiler = ExprCompiler::new();
        for var in &stmt.vars {
            let lval = expr_compiler.lvalue(var)?;
            self.add_statement(IRStatement::Assign(lval, Expr::ReadData))?;
        }

        Ok(())
    }
    fn visit_data(&mut self, stmt: &DataStmt) -> Result<(), CompileError> {
        self.builder.add_data(stmt.vals.iter().map(|v| *v));

        Ok(())
    }

    fn visit_let(&mut self, stmt: &LetStmt) -> Result<(), CompileError> {
        let mut expr_compiler = ExprCompiler::new();
        let lval = expr_compiler.lvalue(&stmt.var)?;
        let expr = expr_compiler.visit_expr(&stmt.expr)?;

        self.add_statement(IRStatement::Assign(lval, expr))?;
        self.add_basic_block_branch();

        Ok(())
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result<(), CompileError> {
        let mut newline = false;

        for part in &stmt.parts {
            let stmt = match part {
                Printable::Label(s) => {
                    newline = true;
                    IRStatement::PrintLabel(s.clone())
                }
                Printable::Advance3 => {
                    newline = false;
                    IRStatement::PrintAdvance3
                }
                Printable::Advance15 => {
                    newline = false;
                    IRStatement::PrintAdvance15
                }
                Printable::Expr(expr) => {
                    let mut expr_compiler = ExprCompiler::new();
                    let expr = expr_compiler.visit_expr(expr)?;
                    newline = true;
                    IRStatement::Print(expr)
                }
            };

            self.add_statement(stmt)?;
        }

        if newline {
            self.add_statement(IRStatement::PrintNewline)?;
        }

        self.add_basic_block_branch();

        Ok(())
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result<(), CompileError> {
        let func = self.current_func();
        let label = self.current_label();

        let gotoline = self.cf_ctx.find_line_index(stmt.goto).unwrap();
        let to_label = self.cf_ctx.get_label(gotoline).unwrap();

        self.builder.add_branch(func, label, to_label);

        Ok(())
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result<(), CompileError> {
        let subline = self.cf_ctx.find_line_index(stmt.goto).unwrap();
        let subname = self.cf_ctx.get_func(subline).unwrap();

        self.add_statement(IRStatement::CallSub(subname))?;
        self.add_basic_block_branch();

        Ok(())
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result<(), CompileError> {
        let mut expr_compiler = ExprCompiler::new();
        let expr = expr_compiler.visit_if(stmt)?;

        let func = self.current_func();
        let label = self.current_label();

        let gotoline = self.cf_ctx.find_line_index(stmt.then).unwrap();
        let to_label = self.cf_ctx.get_label(gotoline).unwrap();

        self.builder.add_conditional_branch(
            func,
            expr,
            label,
            to_label,
            self.next_line_label(),
        );

        Ok(())
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result<(), CompileError> {
        let lval = LV::FnPtr(stmt.func);
        let func = self.cf_ctx.get_def_func(self.line_index).unwrap();

        let mut expr_compiler = ExprCompiler::new();
        let expr = expr_compiler.visit_def(stmt)?;

        self.builder
            .add_return(func, self.current_label(), Some(expr));

        self.add_statement(IRStatement::DefFn(lval, func))?;
        self.add_basic_block_branch();

        Ok(())
    }

    fn visit_rem(&mut self) -> Result<(), CompileError> {
        self.add_basic_block_branch();

        Ok(())
    }

    fn visit_end(&mut self) -> Result<(), CompileError> {
        let func = self.current_func();
        let label = self.current_label();

        if Some(func) != self.main {
            Err(CompileError::Custom("unexpected end"))
        } else {
            self.builder.add_return(func, label, None);
            Ok(())
        }
    }

    fn visit_stop(&mut self) -> Result<(), CompileError> {
        self.visit_end()
    }

    fn visit_return(&mut self) -> Result<(), CompileError> {
        let func = self.current_func();
        let label = self.current_label();

        if Some(func) == self.main {
            Err(CompileError::Custom("unexpected return"))
        } else {
            self.builder.add_return(func, label, None);
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use basic_rs::{Parser, Scanner};
    use indoc::*;

    #[test]
    fn test_no_branching() {
        let program = indoc!(
            "
            10 REM Comment
            20 LET X = 10
            30 LET Y = X + 1
            99 END"
        );
        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut builder = Builder::new();
        let cf_ctx = CfCtx::from_program(&ast).unwrap();

        let mut pass = NonLoopPass::new(&cf_ctx, &mut builder);

        let r = pass.visit_program(&ast);

        assert!(r.is_ok());
    }

    #[test]
    fn test_simple_if() {
        let program = indoc!(
            "
            10 REM Comment
            20 IF X = 10 THEN 99
            30 PRINT \"Ok\"
            99 END"
        );
        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut builder = Builder::new();
        let cf_ctx = CfCtx::from_program(&ast).unwrap();

        let mut pass = NonLoopPass::new(&cf_ctx, &mut builder);

        let r = pass.visit_program(&ast);

        assert!(r.is_ok());
    }

    #[test]
    fn test_simple_goto() {
        let program = indoc!(
            "
            10 REM Comment
            20 GOTO 40
            30 PRINT \"Ok\"
            40 GOTO 30"
        );
        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut builder = Builder::new();
        let cf_ctx = CfCtx::from_program(&ast).unwrap();

        let mut pass = NonLoopPass::new(&cf_ctx, &mut builder);

        let r = pass.visit_program(&ast);

        assert!(r.is_ok());
    }

    #[test]
    fn test_detect_subroutine() {
        let program = indoc!(
            "
            10 LET X = 1
            20 GOSUB 70
            30 GOSUB 70
            40 END
            70 LET X = X + 1
            99 RETURN"
        );
        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut builder = Builder::new();
        let cf_ctx = CfCtx::from_program(&ast).unwrap();

        let mut pass = NonLoopPass::new(&cf_ctx, &mut builder);

        let r = pass.visit_program(&ast);
        let program = builder.build();

        assert!(r.is_ok());
        assert_eq!(program.functions.len(), 2);
    }
}
