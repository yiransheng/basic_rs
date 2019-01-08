use crate::ast::{self, Visitor as AstVisitor};
use crate::ir::{Builder, FunctionName, Label, Statement};
use crate::scanner::SourceMapped;

use super::control_flow_context::CfCtx;
use super::error::CompileError;

pub(super) trait Pass {
    type State;
}

pub(super) struct Compiler<'a, P: Pass> {
    pub(super) cf_ctx: &'a mut CfCtx,
    pub(super) builder: &'a mut Builder,

    pub(super) line_index: usize,
    pub(super) line_no: ast::LineNo,
    pub(super) state: P::State,
}

impl<'a, P> Compiler<'a, P>
where
    P: Pass,
    Compiler<'a, P>: AstVisitor<Result<(), CompileError>>,
{
    pub(super) fn compile(
        &mut self,
        program: &ast::Program,
    ) -> Result<(), SourceMapped<CompileError>> {
        self.visit_program(program).map_err(|err| {
            let line = self.line_index;
            SourceMapped {
                loc: program.statements[line].loc,
                value: err,
            }
        })
    }
}

impl<'a, P> Compiler<'a, P>
where
    P: Pass,
    P::State: Default,
{
    pub(super) fn new(cf_ctx: &'a mut CfCtx, builder: &'a mut Builder) -> Self {
        Compiler {
            cf_ctx,
            builder,
            line_index: 0,
            line_no: ast::LineNo::default(),
            state: P::State::default(),
        }
    }

    pub(super) fn add_statement(
        &mut self,
        stmt: Statement,
    ) -> Result<(), CompileError> {
        self.builder.set_line_no(self.line_no);
        self.builder
            .add_statement(self.current_func()?, self.current_label()?, stmt)
            .map_err(|_| CompileError::Custom("function or block not found"))
    }

    pub(super) fn add_basic_block_branch(
        &mut self,
    ) -> Result<(), CompileError> {
        let current_label = self.current_label()?;
        let next_label = self.next_line_label();

        if let Some(next_label) = next_label {
            if next_label != current_label {
                self.builder.add_branch(
                    self.current_func()?,
                    current_label,
                    next_label,
                );
            }
        }

        Ok(())
    }

    pub(super) fn current_label(&self) -> Result<Label, CompileError> {
        self.cf_ctx.get_label(self.line_index).ok_or_else(|| {
            let line_no = self.cf_ctx.find_line_no(self.line_index);
            CompileError::UnreachableCode(line_no)
        })
    }

    pub(super) fn current_func(&self) -> Result<FunctionName, CompileError> {
        self.cf_ctx.get_func(self.line_index).ok_or_else(|| {
            let line_no = self.cf_ctx.find_line_no(self.line_index);
            CompileError::UnreachableCode(line_no)
        })
    }

    pub(super) fn next_line_label(&self) -> Option<Label> {
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
