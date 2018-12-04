use super::error::CompileError;
use crate::ast::*;
use crate::vm::*;

pub struct LineOrder<'a> {
    chunk: &'a mut Chunk,
    prev_line: usize,
}

impl<'a> LineOrder<'a> {
    pub fn new(chunk: &'a mut Chunk) -> Self {
        LineOrder {
            chunk,
            prev_line: 0,
        }
    }
}

impl<'a> Visitor<Result<(), CompileError>> for LineOrder<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for s in prog.statements.iter() {
            if s.line_no == self.prev_line {
                return Err(CompileError::DuplicatedLines(s.line_no));
            }
            if s.line_no < self.prev_line {
                return Err(CompileError::LinesNotInOrder(self.prev_line, s.line_no));
            }

            self.prev_line = s.line_no;
        }

        Ok(())
    }

    fn visit_let(&mut self, stmt: &LetStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_data(&mut self, stmt: &DataStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result<(), CompileError> {
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
        Ok(())
    }

    fn visit_list(&mut self, list: &List) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_table(&mut self, table: &Table) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expression) -> Result<(), CompileError> {
        Ok(())
    }
}
