use std::collections::VecDeque;

use crate::ast::{Visitor as AstVisitor, *};

use super::control_flow_context::CfCtx;
use super::error::CompileError;
use super::expr_compiler::ExprCompiler;
use super::HasLineState;
use crate::ir::{
    BinaryOp, Builder, Expr, FunctionName, LValue as LV, Label,
    Statement as IRStatement, ValueType,
};

pub struct LoopPass<'a> {
    cf_ctx: &'a mut CfCtx,
    builder: &'a mut Builder,

    line_index: usize,
}

impl<'a> LoopPass<'a> {
    pub fn new(cf_ctx: &'a mut CfCtx, builder: &'a mut Builder) -> Self {
        LoopPass {
            cf_ctx,
            builder,

            line_index: 0,
        }
    }
    fn current_label(&self) -> Result<Label, CompileError> {
        self.cf_ctx.get_label(self.line_index).ok_or_else(|| {
            let line_no = self.cf_ctx.find_line_no(self.line_index);
            CompileError::UnreachableCode(line_no)
        })
    }

    fn current_func(&self) -> Result<FunctionName, CompileError> {
        self.cf_ctx.get_func(self.line_index).ok_or_else(|| {
            let line_no = self.cf_ctx.find_line_no(self.line_index);
            CompileError::UnreachableCode(line_no)
        })
    }

    fn next_line_label(&self) -> Option<Label> {
        match (
            self.current_func(),
            self.cf_ctx.get_func(self.line_index + 1),
        ) {
            (Ok(current_func), Some(next_func))
                if current_func == next_func =>
            {
                self.cf_ctx.get_label(self.line_index + 1)
            }
            _ => None,
        }
    }
}
impl<'a> HasLineState<CompileError> for LoopPass<'a> {
    fn line_state(&self) -> usize {
        self.line_index
    }
}

impl<'a> AstVisitor<Result<(), CompileError>> for LoopPass<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for (i, stmt) in prog.statements.iter().enumerate() {
            self.line_index = i;
            let stmt = match &stmt.statement {
                Stmt::For(stmt) => stmt,
                _ => continue,
            };

            let label = self.current_label()?;
            let func = self.current_func()?;
            let successor_label = self
                .next_line_label()
                .ok_or_else(|| CompileError::ForWithoutNext(stmt.var))?;
            let mut for_compiler = ForCompiler {
                cf_ctx: &mut *self.cf_ctx,
                builder: &mut *self.builder,
                func,
                label,
                successor_label,
            };
            let for_var = stmt.var;
            let mut for_state = Some(for_compiler.visit_for(stmt)?);
            let mut successors: VecDeque<usize> =
                self.cf_ctx.line_successors(i).collect();
            let mut visited: Vec<bool> =
                prog.statements.iter().map(|_| false).collect();

            loop {
                let j = match successors.pop_back() {
                    Some(j) => {
                        if visited[j] {
                            continue;
                        } else {
                            visited[j] = true;
                            j
                        }
                    }
                    None => break,
                };

                self.line_index = j;

                match &prog.statements[j].statement {
                    Stmt::Next(stmt) if stmt.var == for_var => {
                        let label = self.current_label()?;
                        let func = self.current_func()?;
                        let successor_label = self.next_line_label();

                        self.builder.set_line_no(prog.statements[j].line_no);

                        let mut next_compiler = NextCompiler {
                            cf_ctx: &mut *self.cf_ctx,
                            builder: &mut *self.builder,
                            func,
                            label,
                            successor_label,
                            for_state: for_state.take(),
                        };
                        next_compiler.visit_next(stmt)?;
                    }
                    _ => {
                        successors.extend(self.cf_ctx.line_successors(j));
                    }
                }
            }

            if let Some(for_state) = for_state {
                return Err(CompileError::ForWithoutNext(for_state.var));
            }
        }

        Ok(())
    }
}

struct ForState {
    func: FunctionName,
    test: Label,
    body: Label,
    var: Variable,
    step: Expr,
    cond: Expr,
}

struct ForCompiler<'a> {
    cf_ctx: &'a mut CfCtx,
    builder: &'a mut Builder,

    func: FunctionName,
    label: Label,
    successor_label: Label,
}

impl<'a> AstVisitor<Result<ForState, CompileError>> for ForCompiler<'a> {
    fn visit_for(&mut self, stmt: &ForStmt) -> Result<ForState, CompileError> {
        let func = self.func;
        let var = stmt.var;

        let header = self.label;
        let test = self.cf_ctx.add_label();
        let body = self.successor_label;

        let mut expr_compiler = ExprCompiler::new();
        let from = expr_compiler.visit_expr(&stmt.from)?;
        let to = expr_compiler.visit_expr(&stmt.to)?;
        let step = match stmt.step {
            Some(ref step) => expr_compiler.visit_expr(step)?,
            _ => Expr::Const(1.0),
        };

        let step = match step {
            expr @ Expr::Const(_) => expr,
            expr @ _ => {
                let step_local =
                    self.builder.add_local(ValueType::F64, func).map_err(
                        |_| CompileError::Custom("function not found"),
                    )?;
                self.builder
                    .add_statement(
                        func,
                        header,
                        IRStatement::Assign(LV::Local(step_local), expr),
                    )
                    .map_err(|_| CompileError::Custom("block not found"))?;

                Expr::Get(Box::new(LV::Local(step_local)))
            }
        };

        let target = match to {
            expr @ Expr::Const(_) => expr,
            expr @ _ => {
                let target_local = self
                    .builder
                    .add_local(ValueType::F64, func)
                    .map_err(|_| CompileError::Custom("function not found"))?;
                self.builder
                    .add_statement(
                        func,
                        header,
                        IRStatement::Assign(LV::Local(target_local), expr),
                    )
                    .map_err(|_| CompileError::Custom("block not found"))?;

                Expr::Get(Box::new(LV::Local(target_local)))
            }
        };

        // setup
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
        let dir = Expr::Binary(
            BinaryOp::CopySign,
            Box::new(Expr::Const(1.0)),
            Box::new(step.clone()),
        );
        let current = Expr::Get(Box::new(LV::Global(var)));
        let dist =
            Expr::Binary(BinaryOp::Sub, Box::new(current), Box::new(target));
        let mut dist =
            Expr::Binary(BinaryOp::Mul, Box::new(dist), Box::new(dir));

        dist.evaluate_const();

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
            step,
        };

        Ok(for_state)
    }
}

struct NextCompiler<'a> {
    cf_ctx: &'a mut CfCtx,
    builder: &'a mut Builder,
    for_state: Option<ForState>,

    func: FunctionName,
    label: Label,
    successor_label: Option<Label>,
}
impl<'a> AstVisitor<Result<(), CompileError>> for NextCompiler<'a> {
    fn visit_next(&mut self, stmt: &NextStmt) -> Result<(), CompileError> {
        let ForState {
            func,
            cond,
            test,
            body,
            var,
            step,
        } = self
            .for_state
            .take()
            .ok_or_else(|| CompileError::NextWithoutFor(stmt.var))?;

        let current_func = self.func;
        let next_label = self.label;
        let continue_to = match self.successor_label {
            Some(label) => label,
            None => {
                let label = self.cf_ctx.add_label();
                let _ = self.builder.add_block(func, label);
                label
            }
        };

        if stmt.var != var {
            return Err(CompileError::NextWithoutFor(stmt.var));
        }
        if current_func != func {
            return Err(CompileError::NextWithoutFor(stmt.var));
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
                    Box::new(step),
                ),
            ),
        );

        self.builder.add_branch(func, next_label, test);

        Ok(())
    }
}
