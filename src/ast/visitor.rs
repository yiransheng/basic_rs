use super::*;

pub trait Visitor<T> {
    fn visit_program(&mut self, prog: &Program) -> T;

    fn visit_statement(&mut self, stmt: &Statement) -> T {
        match &stmt.statement {
            Stmt::Let(s) => self.visit_let(s),
            Stmt::Read(s) => self.visit_read(s),
            Stmt::Data(s) => self.visit_data(s),
            Stmt::Print(s) => self.visit_print(s),
            Stmt::Goto(s) => self.visit_goto(s),
            Stmt::Gosub(s) => self.visit_gosub(s),
            Stmt::If(s) => self.visit_if(s),
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

    fn visit_let(&mut self, stmt: &LetStmt) -> T;

    fn visit_read(&mut self, stmt: &ReadStmt) -> T;

    fn visit_data(&mut self, stmt: &DataStmt) -> T;

    fn visit_print(&mut self, stmt: &PrintStmt) -> T;

    fn visit_goto(&mut self, stmt: &GotoStmt) -> T;

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> T;

    fn visit_if(&mut self, stmt: &IfStmt) -> T;

    fn visit_for(&mut self, stmt: &ForStmt) -> T;

    fn visit_next(&mut self, stmt: &NextStmt) -> T;

    fn visit_def(&mut self, stmt: &DefStmt) -> T;

    fn visit_dim(&mut self, stmt: &DimStmt) -> T;

    fn visit_rem(&mut self) -> T;

    fn visit_end(&mut self) -> T;

    fn visit_stop(&mut self) -> T;

    fn visit_return(&mut self) -> T;

    fn visit_lvalue(&mut self, lval: &LValue) -> T {
        match lval {
            LValue::Variable(var) => self.visit_variable(var),
            LValue::List(list) => self.visit_list(list),
            LValue::Table(table) => self.visit_table(table),
        }
    }

    fn visit_variable(&mut self, lval: &Variable) -> T;

    fn visit_list(&mut self, list: &List) -> T;

    fn visit_table(&mut self, table: &Table) -> T;

    fn visit_expr(&mut self, table: &Expression) -> T;
}
