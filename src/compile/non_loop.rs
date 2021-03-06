use crate::ast::{Visitor as AstVisitor, *};
use either::Either;

use super::compiler::{Compiler, Pass};
use super::error::CompileError;
use super::expr_compiler::ExprCompiler;
use crate::ir::{
    Expr, FunctionName, LValue as LV, Offset, Statement as IRStatement,
};

pub enum NonLoopPass {}

impl Pass for NonLoopPass {
    type State = Option<FunctionName>;
}

impl<'a> AstVisitor<Result<(), CompileError>> for Compiler<'a, NonLoopPass> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for (line_no, func, entry, ty) in self.cf_ctx.functions() {
            self.builder.set_line_no(line_no);
            self.builder
                .add_function(ty, func, entry)
                .map_err(|_| CompileError::Custom("Function already exist"))?;
        }

        let main_func = self.cf_ctx.get_func(0).unwrap();
        self.state = Some(main_func);
        self.builder
            .set_main(main_func)
            .map_err(|_| CompileError::Custom("Main already set"))?;

        for i in 0..prog.statements.len() {
            let func = self.cf_ctx.get_func(i);
            let label = self.cf_ctx.get_label(i);

            if let (Some(func), Some(label)) = (func, label) {
                let _ = self.builder.add_block(func, label);
            }
        }

        for (i, stmt) in prog.statements.iter().enumerate() {
            self.line_index = i;
            self.line_no = stmt.line_no;

            let func = self.cf_ctx.get_func(i);
            let label = self.cf_ctx.get_label(i);

            if let (Some(_), Some(_)) = (func, label) {
                self.visit_statement(stmt)?;
            }
        }

        Ok(())
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        let mut expr_compiler = ExprCompiler::new();
        for var in &stmt.vars {
            let lval = expr_compiler.lvalue(var)?;
            self.add_statement(IRStatement::Assign(lval, Expr::ReadData))?;
        }
        self.add_basic_block_branch()
    }
    fn visit_data(&mut self, _stmt: &DataStmt) -> Result<(), CompileError> {
        self.add_basic_block_branch()
    }

    fn visit_input(&mut self, stmt: &InputStmt) -> Result<(), CompileError> {
        let mut expr_compiler = ExprCompiler::new();

        for part in &stmt.prompts {
            let stmt = match part {
                Printable::Label(s) => {
                    let (offset, length) = self.builder.add_string_label(&*s);
                    IRStatement::PrintLabel(offset, length)
                }
                Printable::Advance3 => IRStatement::PrintAdvance3,
                Printable::Advance15 => IRStatement::PrintAdvance15,
                Printable::Expr(_) => unreachable!(),
            };

            self.add_statement(stmt)?;
        }

        self.add_statement(IRStatement::PrintNewline)?;

        for var in &stmt.vars {
            let lval = expr_compiler.lvalue(var)?;
            self.add_statement(IRStatement::Assign(lval, Expr::Input))?;
        }

        self.add_basic_block_branch()
    }

    fn visit_let(&mut self, stmt: &LetStmt) -> Result<(), CompileError> {
        let mut expr_compiler = ExprCompiler::new();
        let lval = expr_compiler.lvalue(&stmt.var)?;
        let expr = expr_compiler.visit_expr(&stmt.expr)?;

        self.add_statement(IRStatement::Assign(lval, expr))?;
        self.add_basic_block_branch()
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result<(), CompileError> {
        let mut newline = true;

        for part in &stmt.parts {
            let stmt = match part {
                Printable::Label(s) => {
                    newline = true;
                    let (offset, length) = self.builder.add_string_label(&*s);
                    IRStatement::PrintLabel(offset, length)
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

        self.add_basic_block_branch()
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result<(), CompileError> {
        let func = self.current_func()?;
        let label = self.current_label()?;

        let gotoline = self.cf_ctx.find_line_index(stmt.goto).unwrap();
        let to_label = self.cf_ctx.get_label(gotoline).unwrap();

        self.builder.add_branch(func, label, to_label);

        Ok(())
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result<(), CompileError> {
        let subline = self.cf_ctx.find_line_index(stmt.goto).unwrap();
        let subname = self.cf_ctx.get_func(subline).unwrap();

        self.add_statement(IRStatement::CallSub(subname))?;
        self.add_basic_block_branch()
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result<(), CompileError> {
        let mut expr_compiler = ExprCompiler::new();
        let expr = expr_compiler.visit_if(stmt)?;

        let func = self.current_func()?;
        let label = self.current_label()?;

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

    fn visit_for(&mut self, _stmt: &ForStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_next(&mut self, _stmt: &NextStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result<(), CompileError> {
        let lval = LV::FnPtr(stmt.func);
        let func = self
            .cf_ctx
            .get_def_func(self.line_index)
            .ok_or_else(|| CompileError::FunctionNotDefined(stmt.func))?;

        let mut expr_compiler = ExprCompiler::new();
        let expr = expr_compiler.visit_def(stmt)?;

        self.builder
            .add_return(func, self.current_label()?, Some(expr));

        self.add_statement(IRStatement::DefFn(lval, func))?;
        self.add_basic_block_branch()
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result<(), CompileError> {
        let mut expr_compiler = ExprCompiler::new();

        for dim in &stmt.dims {
            match dim {
                Either::Left(List { var, subscript }) => {
                    let lval = LV::ArrPtr(*var, Offset::OneD(Expr::Const(0.0)));
                    let size = expr_compiler.visit_expr(subscript)?;
                    self.add_statement(IRStatement::Alloc1d(lval, size))?;
                }
                Either::Right(Table { var, subscript }) => {
                    let lval = LV::ArrPtr(
                        *var,
                        Offset::TwoD(Expr::Const(0.0), Expr::Const(0.0)),
                    );
                    let nrow = expr_compiler.visit_expr(&subscript.0)?;
                    let ncol = expr_compiler.visit_expr(&subscript.1)?;
                    self.add_statement(IRStatement::Alloc2d(lval, nrow, ncol))?;
                }
            }
        }

        self.add_basic_block_branch()?;

        Ok(())
    }

    fn visit_rem(&mut self) -> Result<(), CompileError> {
        self.add_basic_block_branch()?;

        Ok(())
    }

    fn visit_end(&mut self) -> Result<(), CompileError> {
        let func = self.current_func()?;
        let label = self.current_label()?;

        if Some(func) != self.state {
            Err(CompileError::EndInSubroutine)
        } else {
            self.builder.add_return(func, label, None);
            Ok(())
        }
    }

    fn visit_stop(&mut self) -> Result<(), CompileError> {
        self.visit_end()
    }

    fn visit_return(&mut self) -> Result<(), CompileError> {
        let func = self.current_func()?;
        let label = self.current_label()?;

        if Some(func) == self.state {
            Err(CompileError::ReturnInMain)
        } else {
            self.builder.add_return(func, label, None);
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::control_flow_context::CfCtx;
    use super::*;
    use crate::ir::Builder;
    use crate::{Parser, Scanner};
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
        let mut cf_ctx = CfCtx::from_program(&ast).unwrap();

        let mut pass: Compiler<NonLoopPass> =
            Compiler::new(&mut cf_ctx, &mut builder);

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
        let mut cf_ctx = CfCtx::from_program(&ast).unwrap();

        let mut pass: Compiler<NonLoopPass> =
            Compiler::new(&mut cf_ctx, &mut builder);

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
        let mut cf_ctx = CfCtx::from_program(&ast).unwrap();

        let mut pass: Compiler<NonLoopPass> =
            Compiler::new(&mut cf_ctx, &mut builder);

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
        let mut cf_ctx = CfCtx::from_program(&ast).unwrap();

        let mut pass: Compiler<NonLoopPass> =
            Compiler::new(&mut cf_ctx, &mut builder);

        let r = pass.visit_program(&ast);
        let program = builder.build();

        assert!(r.is_ok());
        assert_eq!(program.functions.len(), 2);
    }
}
