use super::error::CompileError;
use crate::ast::*;
use crate::ir::{Instruction, InstructionKind, Visitor as IRVisitor};
use crate::vm::*;

pub struct PrepareData<V> {
    ir_visitor: V,
    read_count: usize,
    data_count: usize,
    line: usize,
}

impl<V: IRVisitor> PrepareData<V> {
    pub fn new(ir_visitor: V) -> Self {
        PrepareData {
            ir_visitor,
            read_count: 0,
            data_count: 0,
            line: 0,
        }
    }
}

impl<V: IRVisitor> Visitor<Result<(), CompileError>> for PrepareData<V>
where
    V::Error: Into<CompileError>,
{
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        for s in prog.statements.iter() {
            self.line = s.line_no;
            self.visit_statement(s)?;
        }

        Ok(())
    }

    fn visit_let(&mut self, _stmt: &LetStmt) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        self.read_count += stmt.vars.len();
        Ok(())
    }

    fn visit_data(&mut self, stmt: &DataStmt) -> Result<(), CompileError> {
        self.data_count += stmt.vals.len();
        for val in stmt.vals.iter() {
            // self.chunk.write_opcode(OpCode::Constant, self.line);
            // self.chunk.add_operand(*val, self.line);
            let kind = InstructionKind::Data(*val);
            self.ir_visitor
                .visit_instruction(Instruction {
                    label: None,
                    line_no: self.line,
                    kind,
                })
                .map_err(V::Error::into)?;
        }

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
