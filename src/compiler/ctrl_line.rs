use int_hash::IntHashSet;

use crate::ast::*;

#[derive(Debug)]
pub struct CtrlLineSet {
    lines: IntHashSet<usize>,
}

impl CtrlLineSet {
    pub fn new() -> Self {
        CtrlLineSet {
            lines: IntHashSet::default(),
        }
    }
}

impl Visitor<()> for CtrlLineSet {
    fn visit_program(&mut self, prog: &Program) {
        for s in &prog.statements {
            self.visit_statement(s);
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        let line_no = stmt.line_no;

        match stmt.statement {
            Stmt::Goto(ref s) => self.visit_goto(s),
            Stmt::Gosub(ref s) => {
                // return to this line
                self.lines.insert(line_no);
                self.visit_gosub(s);
            }
            Stmt::If(ref s) => self.visit_if(s),
            Stmt::For(ref s) => {
                self.lines.insert(line_no);
            }
            _ => {}
        }
    }

    fn visit_let(&mut self, stmt: &LetStmt) {}

    fn visit_read(&mut self, stmt: &ReadStmt) {}

    fn visit_data(&mut self, stmt: &DataStmt) {}

    fn visit_print(&mut self, stmt: &PrintStmt) {}

    fn visit_goto(&mut self, stmt: &GotoStmt) {
        self.lines.insert(stmt.goto);
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) {
        self.lines.insert(stmt.goto);
    }

    fn visit_if(&mut self, stmt: &IfStmt) {
        self.lines.insert(stmt.then);
    }

    fn visit_for(&mut self, stmt: &ForStmt) {}

    fn visit_next(&mut self, stmt: &NextStmt) {}

    fn visit_def(&mut self, stmt: &DefStmt) {}

    fn visit_dim(&mut self, stmt: &DimStmt) {}

    fn visit_rem(&mut self) {}

    fn visit_end(&mut self) {}

    fn visit_stop(&mut self) {}

    fn visit_return(&mut self) {}

    // needed?

    fn visit_lvalue(&mut self, lval: &LValue) {}

    fn visit_variable(&mut self, lval: &Variable) {}

    fn visit_list(&mut self, list: &List) {}

    fn visit_table(&mut self, table: &Table) {}

    fn visit_expr(&mut self, table: &Expression) {}
}

#[cfg(test)]
mod tests {
    use indoc::*;

    use super::*;
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    #[test]
    fn test_collects_control_lines() {
        let program = indoc!(
            "
            10 GOTO 30
            20 GOSUB 40
            30 IF 1 < 0 THEN 60
            40 FOR X = 1 TO 10
            50   PRINT X
            60 NEXT X
            70 RETURN"
        );
        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut lines = CtrlLineSet::new();
        let expected: IntHashSet<usize> = [40, 60, 30, 20].iter().map(|d| *d).collect();

        lines.visit_program(&ast);

        assert_eq!(&expected, &lines.lines);
    }

}
