use crate::ast::*;
use crate::vm::value::{Static, Variant};
use crate::vm::*;

struct CompileState {
    assign: bool,
    line: LineNo,
}

pub enum Error {
    CompileError,
}

pub struct Compiler<'a> {
    state: CompileState,
    chunk: &'a mut Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(chunk: &'a mut Chunk) -> Self {
        Compiler {
            chunk,
            state: CompileState {
                assign: false,
                line: 0,
            },
        }
    }
}

impl<'a> Visitor<()> for Compiler<'a> {
    fn visit_program(&mut self, prog: &Program) {
        for s in &prog.statements {
            self.visit_statement(s);
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        let line_no = stmt.line_no;

        self.state.line = line_no;

        match &stmt.statement {
            Stmt::Let(ref s) => {
                self.visit_let(s);
            }
            Stmt::Print(ref s) => {
                self.visit_print(s);
            }
            Stmt::End => {
                self.visit_end();
            }
            _ => {}
        }
    }

    fn visit_let(&mut self, stmt: &LetStmt) {
        self.visit_expr(&stmt.expr);
        self.state.assign = true;
        self.visit_lvalue(&stmt.var);
        self.state.assign = false;
    }

    fn visit_read(&mut self, stmt: &ReadStmt) {}

    fn visit_data(&mut self, stmt: &DataStmt) {}

    fn visit_print(&mut self, stmt: &PrintStmt) {
        for part in &stmt.parts {
            match part {
                Printable::Expr(ref expr) => {
                    self.visit_expr(expr);
                    self.chunk.write(OP_PRINT, self.state.line);
                }
                _ => {}
            }
        }
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) {}

    fn visit_gosub(&mut self, stmt: &GosubStmt) {}

    fn visit_if(&mut self, stmt: &IfStmt) {}

    fn visit_for(&mut self, stmt: &ForStmt) {}

    fn visit_next(&mut self, stmt: &NextStmt) {}

    fn visit_def(&mut self, stmt: &DefStmt) {}

    fn visit_dim(&mut self, stmt: &DimStmt) {}

    fn visit_rem(&mut self) {}

    fn visit_end(&mut self) {
        self.chunk.write(OP_STOP, self.state.line);
    }

    fn visit_stop(&mut self) {}

    fn visit_return(&mut self) {}

    // needed?

    fn visit_lvalue(&mut self, lval: &LValue) {
        match lval {
            LValue::Variable(ref var) => self.visit_variable(var),
            _ => panic!(),
        }
    }

    fn visit_variable(&mut self, lval: &Variable) {
        if self.state.assign {
            self.chunk.write(OP_SET_GLOBAL, self.state.line);
        } else {
            self.chunk.write(OP_GET_GLOBAL, self.state.line);
        }

        let offset = self.chunk.add_constant(Variant::from(*lval));
        self.chunk.write(offset as u8, self.state.line);
    }

    fn visit_list(&mut self, list: &List) {}

    fn visit_table(&mut self, table: &Table) {}

    fn visit_expr(&mut self, expr: &Expression) {
        if self.state.assign {
            panic!();
        }
        match expr {
            Expression::Lit(n) => {
                self.chunk
                    .write_constant(Variant::Number(*n), self.state.line);
            }
            Expression::Var(ref v) => {
                self.visit_lvalue(v);
            }
            Expression::Neg(ref expr) => {
                self.visit_expr(expr);
                self.chunk.write(OP_NEGATE, self.state.line);
            }
            Expression::Add(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write(OP_ADD, self.state.line);
            }
            Expression::Sub(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write(OP_SUBTRACT, self.state.line);
            }
            Expression::Mul(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write(OP_MULTIPLY, self.state.line);
            }
            Expression::Div(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write(OP_DIVIDE, self.state.line);
            }
            Expression::Pow(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write(OP_POWER, self.state.line);
            }
            _ => panic!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::*;

    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::vm::{Chunk, VM};

    #[test]
    fn test_simple() {
        let program = indoc!(
            "
            10 LET X = 10
            20 LET X = X + 20
            30 PRINT X, X, X
            40 END"
        );

        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(&mut chunk);

        compiler.visit_program(&ast);

        let mut vm = VM::new(chunk);
        vm.run();

        assert!(false);
    }
}
