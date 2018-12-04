use either::Either;
use int_hash::IntHashMap;

use super::anon_var::AnonVarGen;
use crate::ast::*;
use crate::vm::*;

struct CompileState {
    assign: bool,
    line: LineNo,
    var_gen: AnonVarGen,
}

struct ForState {
    var: Variable,
    step: Variable,
    to: Variable,
    start_code_point: usize,
}

pub enum Error {
    CompileError,
}

pub struct Compiler<'a> {
    state: CompileState,
    line_addr_map: IntHashMap<LineNo, usize>,
    jumps: IntHashMap<LineNo, u16>,
    for_states: IntHashMap<Variable, ForState>,
    chunk: &'a mut Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(chunk: &'a mut Chunk) -> Self {
        Compiler {
            chunk,
            line_addr_map: IntHashMap::default(),
            jumps: IntHashMap::default(),
            for_states: IntHashMap::default(),
            state: CompileState {
                assign: false,
                line: 0,
                var_gen: AnonVarGen::new(),
            },
        }
    }
    fn assigning<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Self),
    {
        self.state.assign = true;
        f(self);
        self.state.assign = false;
    }
}

impl<'a> Visitor<()> for Compiler<'a> {
    fn visit_program(&mut self, prog: &Program) {
        for s in &prog.statements {
            self.visit_statement(s);
        }

        for (line_no, index) in self.jumps.iter() {
            let jp = self.line_addr_map.get(line_no).unwrap();
            self.chunk.set_operand(*index, JumpPoint(*jp));
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        let line_no = stmt.line_no;

        self.state.line = line_no;
        self.line_addr_map.insert(line_no, self.chunk.len());

        match &stmt.statement {
            Stmt::Let(ref s) => {
                self.visit_let(s);
            }
            Stmt::Goto(ref s) => {
                self.visit_goto(s);
            }
            Stmt::If(ref s) => {
                self.visit_if(s);
            }
            Stmt::For(ref s) => {
                self.visit_for(s);
            }
            Stmt::Next(ref s) => {
                self.visit_next(s);
            }
            Stmt::Print(ref s) => {
                self.visit_print(s);
            }
            Stmt::End => {
                self.visit_end();
            }
            Stmt::Rem => {
                self.visit_rem();
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
                    self.chunk.write_opcode(OpCode::Print, self.state.line);
                }
                _ => {}
            }
        }
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) {
        self.chunk.write_opcode(OpCode::Jump, self.state.line);
        let jp_index = self.chunk.add_operand(JumpPoint(0), self.state.line);
        self.jumps.insert(stmt.goto, jp_index);
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) {}

    fn visit_if(&mut self, stmt: &IfStmt) {
        self.visit_expr(&stmt.lhs);
        self.visit_expr(&stmt.rhs);

        match stmt.op {
            Relop::Equal => {
                self.chunk.write_opcode(OpCode::Equal, self.state.line);
            }
            Relop::NotEqual => {
                self.chunk.write_opcode(OpCode::Equal, self.state.line);
                self.chunk.write_opcode(OpCode::Not, self.state.line);
            }
            Relop::Less => {
                self.chunk.write_opcode(OpCode::Less, self.state.line);
            }
            Relop::Greater => {
                self.chunk.write_opcode(OpCode::Greater, self.state.line);
            }
            Relop::LessEqual => {
                self.chunk.write_opcode(OpCode::Greater, self.state.line);
                self.chunk.write_opcode(OpCode::Not, self.state.line);
            }
            Relop::GreaterEqual => {
                self.chunk.write_opcode(OpCode::Less, self.state.line);
                self.chunk.write_opcode(OpCode::Not, self.state.line);
            }
        }

        self.chunk.write_opcode(OpCode::CondJump, self.state.line);
        let jp_index = self.chunk.add_operand(JumpPoint(0), self.state.line);
        self.jumps.insert(stmt.then, jp_index);
    }

    fn visit_for(&mut self, stmt: &ForStmt) {
        self.visit_expr(&stmt.from);
        self.assigning(|this| {
            this.visit_variable(&stmt.var);
        });

        let step_var = self.state.var_gen.next_anonymous();
        let to_var = self.state.var_gen.next_anonymous();

        self.visit_expr(&stmt.to);
        self.assigning(|this| {
            this.visit_variable(&to_var);
        });

        match stmt.step {
            Some(ref step_expr) => {
                self.visit_expr(step_expr);
            }
            None => {
                self.chunk.write_opcode(OpCode::Constant, self.state.line);
                self.chunk.add_operand(1.0, self.state.line);
            }
        }
        self.assigning(|this| {
            this.visit_variable(&step_var);
        });

        let for_state = ForState {
            var: stmt.var,
            step: step_var,
            to: to_var,
            start_code_point: self.chunk.len(),
        };

        self.for_states.insert(stmt.var, for_state);
    }

    fn visit_next(&mut self, stmt: &NextStmt) {
        // TODO: error handling
        let for_state = self.for_states.remove(&stmt.var).unwrap();
        let step_var = for_state.step;
        let to_var = for_state.to;
        let loop_start = for_state.start_code_point;

        self.visit_variable(&step_var);

        self.chunk.write_opcode(OpCode::Dup, self.state.line);
        self.chunk.write_opcode(OpCode::Sign, self.state.line);
        self.chunk.write_opcode(OpCode::Swap, self.state.line);
        self.visit_variable(&stmt.var);
        self.chunk.write_opcode(OpCode::Add, self.state.line);
        self.chunk.write_opcode(OpCode::Dup, self.state.line);

        self.assigning(|this| {
            this.visit_variable(&stmt.var);
        });
        self.visit_variable(&to_var);
        // value stack: [<step sign>, current, to]
        self.chunk.write_opcode(OpCode::Sub, self.state.line);
        self.chunk.write_opcode(OpCode::Sign, self.state.line);
        self.chunk.write_opcode(OpCode::Sub, self.state.line);
        self.chunk.write_opcode(OpCode::CondJump, self.state.line);
        self.chunk
            .add_operand(JumpPoint(loop_start), self.state.line);
    }

    fn visit_def(&mut self, stmt: &DefStmt) {}

    fn visit_dim(&mut self, stmt: &DimStmt) {}

    fn visit_rem(&mut self) {
        self.chunk.write_opcode(OpCode::Noop, self.state.line);
    }

    fn visit_end(&mut self) {
        self.chunk.write_opcode(OpCode::Stop, self.state.line);
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
            self.chunk.write_opcode(OpCode::SetGlobal, self.state.line);
        } else {
            self.chunk.write_opcode(OpCode::GetGlobal, self.state.line);
        }

        self.chunk
            .add_inline_oprerand(lval.clone(), self.state.line);
    }

    fn visit_list(&mut self, list: &List) {}

    fn visit_table(&mut self, table: &Table) {}

    fn visit_expr(&mut self, expr: &Expression) {
        if self.state.assign {
            panic!();
        }
        match expr {
            Expression::Lit(n) => {
                self.chunk.write_opcode(OpCode::Constant, self.state.line);
                self.chunk.add_operand(*n, self.state.line);
            }
            Expression::Var(ref v) => {
                self.visit_lvalue(v);
            }
            Expression::Neg(ref expr) => {
                self.visit_expr(expr);
                self.chunk.write_opcode(OpCode::Negate, self.state.line);
            }
            Expression::Add(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Add, self.state.line);
            }
            Expression::Sub(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Sub, self.state.line);
            }
            Expression::Mul(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Mul, self.state.line);
            }
            Expression::Div(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Div, self.state.line);
            }
            Expression::Pow(ref lhs, ref rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.chunk.write_opcode(OpCode::Pow, self.state.line);
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
            20 LET Y = 20
            21 FOR I = 1 TO 10 STEP 2
            22   PRINT I
            23 NEXT I
            24 FOR I = 10 TO 1 STEP -1
            25   PRINT I
            26 NEXT I
            24 PRINT I
            24 PRINT 77777
            30 IF X < Y THEN 50
            40 PRINT X
            50 REM IGNORE ME
            55 PRINT Y
            55 IF X > Y THEN 10
            60 END"
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
