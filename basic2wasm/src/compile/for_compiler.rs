use std::collections::VecDeque;

use basic_rs::ast::{Visitor as AstVisitor, *};

use super::control_flow_context::CfCtx;
use super::error::CompileError;
use super::expr_compiler::ExprCompiler;
use crate::ir::{
    BinaryOp, Builder, Expr, FunctionName, LValue as LV, Label,
    Statement as IRStatement, ValueType,
};

struct ForState {
    func: FunctionName,
    test: Label,
    body: Label,
    var: Variable,
    step_local: usize,
    cond: Expr,
}

pub struct LoopPass<'a> {
    cf_ctx: &'a mut CfCtx,
    builder: &'a mut Builder,

    line_index: usize,
    stack: VecDeque<ForState>,
}

impl<'a> LoopPass<'a> {
    pub fn new(cf_ctx: &'a mut CfCtx, builder: &'a mut Builder) -> Self {
        LoopPass {
            cf_ctx,
            builder,

            line_index: 0,
            stack: VecDeque::new(),
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

impl<'a> AstVisitor<Result<(), CompileError>> for LoopPass<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for (i, stmt) in prog.statements.iter().enumerate() {
            self.line_index = i;
            match &stmt.statement {
                Stmt::For(stmt) => {
                    self.visit_for(stmt)?;
                }
                Stmt::Next(stmt) => {
                    self.visit_next(stmt)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result<(), CompileError> {
        let func = self.current_func();
        let var = stmt.var;

        let header = self.current_label();
        let test = self.cf_ctx.add_label();
        let body = self
            .next_line_label()
            .ok_or_else(|| CompileError::Custom("missing for body"))?;

        let mut expr_compiler = ExprCompiler::new();
        let from = expr_compiler.visit_expr(&stmt.from)?;
        let to = expr_compiler.visit_expr(&stmt.to)?;
        let step = match stmt.step {
            Some(ref step) => expr_compiler.visit_expr(step)?,
            _ => Expr::Const(1.0),
        };

        let step_local = self
            .builder
            .add_local(ValueType::F64, func)
            .map_err(|_| CompileError::Custom("function not found"))?;
        let target_local = self
            .builder
            .add_local(ValueType::F64, func)
            .map_err(|_| CompileError::Custom("function not found"))?;

        // setup
        self.builder
            .add_statement(
                func,
                header,
                IRStatement::Assign(LV::Local(step_local), step),
            )
            .map_err(|_| CompileError::Custom("block not found"))?;
        self.builder
            .add_statement(
                func,
                header,
                IRStatement::Assign(LV::Local(target_local), to),
            )
            .map_err(|_| CompileError::Custom("block not found"))?;
        self.builder
            .add_statement(
                func,
                header,
                IRStatement::Assign(LV::Global(var), from),
            )
            .map_err(|_| CompileError::Custom("block not found"))?;

        let _ = self.builder.add_block(func, test);
        self.builder.add_branch(func, header, test);

        // condition test
        let step = Expr::Get(Box::new(LV::Local(step_local)));
        let dir = Expr::Binary(
            BinaryOp::CopySign,
            Box::new(Expr::Const(1.0)),
            Box::new(step),
        );
        let target = Expr::Get(Box::new(LV::Local(target_local)));
        let current = Expr::Get(Box::new(LV::Global(var)));
        let dist =
            Expr::Binary(BinaryOp::Sub, Box::new(current), Box::new(target));
        let dist = Expr::Binary(BinaryOp::Mul, Box::new(dist), Box::new(dir));
        let done = Expr::Binary(
            BinaryOp::Greater,
            Box::new(dist),
            Box::new(Expr::Const(0.0)),
        );
        let for_state = ForState {
            func,
            cond: done,
            test,
            body,
            var,
            step_local,
        };

        self.stack.push_back(for_state);

        Ok(())
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result<(), CompileError> {
        let ForState {
            func,
            cond,
            test,
            body,
            var,
            step_local,
        } = self
            .stack
            .pop_back()
            .ok_or_else(|| CompileError::Custom("next without for"))?;

        let current_func = self.current_func();
        let next_label = self.current_label();
        let continue_to = match self.next_line_label() {
            Some(label) => label,
            None => {
                let label = self.cf_ctx.add_label();
                let _ = self.builder.add_block(func, label);
                label
            }
        };

        if stmt.var != var {
            return Err(CompileError::Custom("next without for"));
        }
        if current_func != func {
            return Err(CompileError::Custom("next without for"));
        }

        self.builder.add_conditional_branch(
            func,
            cond,
            test,
            continue_to,
            Some(body),
        );

        let _ = self.builder.add_statement(
            func,
            next_label,
            IRStatement::Assign(
                LV::Global(var),
                Expr::Binary(
                    BinaryOp::Add,
                    Box::new(Expr::Get(Box::new(LV::Global(var)))),
                    Box::new(Expr::Get(Box::new(LV::Local(step_local)))),
                ),
            ),
        );

        self.builder.add_branch(func, next_label, test);

        Ok(())
    }
}
