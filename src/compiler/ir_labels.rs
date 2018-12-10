use rustc_hash::FxHashMap;

use super::error::CompileError;
use crate::ast::*;
use crate::ir::{Label, LabelIdGen};

pub struct IrLabels<'a> {
    label_mapping: &'a mut FxHashMap<LineNo, Label>,
    id_gen: &'a mut LabelIdGen,
}

impl<'a> IrLabels<'a> {
    pub fn new(
        id_gen: &'a mut LabelIdGen,
        label_mapping: &'a mut FxHashMap<LineNo, Label>,
    ) -> Self {
        IrLabels {
            id_gen,
            label_mapping,
        }
    }
}

impl<'a> Visitor<Result<(), CompileError>> for IrLabels<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for s in prog.statements.iter() {
            self.visit_statement(s)?;
        }

        Ok(())
    }

    fn visit_let(&mut self, _stmt: &LetStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_read(&mut self, _stmt: &ReadStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_data(&mut self, _stmt: &DataStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_print(&mut self, _stmt: &PrintStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result<(), CompileError> {
        if !self.label_mapping.contains_key(&stmt.goto) {
            let label = self.id_gen.next_id();
            self.label_mapping.insert(stmt.goto, label);
        }

        Ok(())
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result<(), CompileError> {
        if !self.label_mapping.contains_key(&stmt.goto) {
            let label = self.id_gen.next_id();
            self.label_mapping.insert(stmt.goto, label);
        }

        Ok(())
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result<(), CompileError> {
        if !self.label_mapping.contains_key(&stmt.then) {
            let label = self.id_gen.next_id();
            self.label_mapping.insert(stmt.then, label);
        }

        Ok(())
    }

    fn visit_for(&mut self, _stmt: &ForStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_next(&mut self, _stmt: &NextStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_def(&mut self, _stmt: &DefStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_dim(&mut self, _stmt: &DimStmt) -> Result<(), CompileError> {
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

    fn visit_list(&mut self, _list: &List) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_table(&mut self, _table: &Table) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_expr(&mut self, _expr: &Expression) -> Result<(), CompileError> {
        Ok(())
    }
}
