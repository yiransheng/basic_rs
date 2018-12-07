use super::error::CompileError;
use crate::ast::*;
use crate::vm::*;

pub struct LineOrder<'a> {
    //TODO: not used remove?
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

    fn visit_goto(&mut self, _stmt: &GotoStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_gosub(&mut self, _stmt: &GosubStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_if(&mut self, _stmt: &IfStmt) -> Result<(), CompileError> {
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
