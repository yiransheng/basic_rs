use super::*;

pub trait Visitor<T> {
    fn visit_program(&mut self, _prog: &Program) -> T {
        unimplemented! {}
    }

    fn visit_statement(&mut self, stmt: &Statement) -> T {
        match &stmt.statement {
            Stmt::Let(s) => self.visit_let(s),
            Stmt::Read(s) => self.visit_read(s),
            Stmt::Data(s) => self.visit_data(s),
            Stmt::Print(s) => self.visit_print(s),
            Stmt::Goto(s) => self.visit_goto(s),
            Stmt::Gosub(s) => self.visit_gosub(s),
            Stmt::If(s) => self.visit_if(s),
            Stmt::Input(s) => self.visit_input(s),
            Stmt::For(s) => self.visit_for(s),
            Stmt::Next(s) => self.visit_next(s),
            Stmt::Def(s) => self.visit_def(s),
            Stmt::Dim(s) => self.visit_dim(s),
            Stmt::End => self.visit_end(),
            Stmt::Rem => self.visit_rem(),
            Stmt::Stop => self.visit_stop(),
            Stmt::Return => self.visit_return(),
        }
    }

    fn visit_let(&mut self, _stmt: &LetStmt) -> T {
        unimplemented!()
    }

    fn visit_read(&mut self, _stmt: &ReadStmt) -> T {
        unimplemented!()
    }

    fn visit_data(&mut self, _stmt: &DataStmt) -> T {
        unimplemented!()
    }

    fn visit_print(&mut self, _stmt: &PrintStmt) -> T {
        unimplemented!()
    }

    fn visit_goto(&mut self, _stmt: &GotoStmt) -> T {
        unimplemented!()
    }

    fn visit_gosub(&mut self, _stmt: &GosubStmt) -> T {
        unimplemented!()
    }

    fn visit_if(&mut self, _stmt: &IfStmt) -> T {
        unimplemented!()
    }

    fn visit_input(&mut self, _stmt: &InputStmt) -> T {
        unimplemented!()
    }

    fn visit_for(&mut self, _stmt: &ForStmt) -> T {
        unimplemented!()
    }

    fn visit_next(&mut self, _stmt: &NextStmt) -> T {
        unimplemented!()
    }

    fn visit_def(&mut self, _stmt: &DefStmt) -> T {
        unimplemented!()
    }

    fn visit_dim(&mut self, _stmt: &DimStmt) -> T {
        unimplemented!()
    }

    fn visit_rem(&mut self) -> T {
        unimplemented!()
    }

    fn visit_end(&mut self) -> T {
        unimplemented!()
    }

    fn visit_stop(&mut self) -> T {
        unimplemented!()
    }

    fn visit_return(&mut self) -> T {
        unimplemented!()
    }

    fn visit_lvalue(&mut self, lval: &LValue) -> T {
        match lval {
            LValue::Variable(var) => self.visit_variable(var),
            LValue::List(list) => self.visit_list(list),
            LValue::Table(table) => self.visit_table(table),
        }
    }

    fn visit_variable(&mut self, _lval: &Variable) -> T {
        unimplemented!()
    }

    fn visit_list(&mut self, _list: &List) -> T {
        unimplemented!()
    }

    fn visit_table(&mut self, _table: &Table) -> T {
        unimplemented!()
    }

    fn visit_expr(&mut self, _table: &Expression) -> T {
        unimplemented!()
    }
}
