use either::Either;
use int_hash::IntHashMap;

use crate::ast::*;
use crate::vm::*;

use super::anon_var::AnonVarGen;
use super::array_dims::ArrayDims;
use super::data::PrepareData;
use super::error::CompileError;
use super::func_compiler::FuncCompiler;
use super::line_order::LineOrder;

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
    fn assigning<T, F>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let prev_assign = self.state.assign;
        self.state.assign = true;
        let ret = f(self);
        self.state.assign = prev_assign;
        ret
    }
    fn evaluating<T, F>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let prev_assign = self.state.assign;
        self.state.assign = false;
        let ret = f(self);
        self.state.assign = prev_assign;
        ret
    }
}

type Result = ::std::result::Result<(), CompileError>;

impl<'a> Visitor<Result> for Compiler<'a> {
    fn visit_program(&mut self, prog: &Program) -> Result {
        let mut line_order = LineOrder::new(self.chunk);
        line_order.visit_program(prog)?;

        let mut array_dims = ArrayDims::new(self.chunk);
        array_dims.visit_program(prog)?;

        let mut prep_data = PrepareData::new(self.chunk);
        prep_data.visit_program(prog)?;

        for s in &prog.statements {
            self.visit_statement(s)?;
        }

        for (line_no, index) in self.jumps.iter() {
            let jp = self
                .line_addr_map
                .get(line_no)
                .ok_or(CompileError::Custom("Don't know where to jump"))?;
            self.chunk.set_operand(*index, JumpPoint(*jp));
        }

        Ok(())
    }

    fn visit_statement(&mut self, stmt: &Statement) -> Result {
        let line_no = stmt.line_no;

        self.state.line = line_no;
        self.line_addr_map.insert(line_no, self.chunk.len());

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

    fn visit_let(&mut self, stmt: &LetStmt) -> Result {
        self.visit_expr(&stmt.expr)?;
        self.assigning(|this| this.visit_lvalue(&stmt.var))
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result {
        self.assigning(|this| {
            for var in &stmt.vars {
                match this.visit_lvalue(var) {
                    Ok(_) => {}
                    Err(e) => return Err(e),
                }
            }

            Ok(())
        })
    }

    fn visit_data(&mut self, _stmt: &DataStmt) -> Result {
        self.chunk.write_opcode(OpCode::Noop, self.state.line);
        Ok(())
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result {
        for part in &stmt.parts {
            match part {
                Printable::Expr(ref expr) => {
                    self.visit_expr(expr)?;
                    self.chunk.write_opcode(OpCode::Print, self.state.line);
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result {
        self.chunk.write_opcode(OpCode::Jump, self.state.line);
        let jp_index = self.chunk.add_operand(JumpPoint(0), self.state.line);
        self.jumps.insert(stmt.goto, jp_index);

        Ok(())
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result {
        self.chunk.write_opcode(OpCode::Subroutine, self.state.line);
        let jp_index = self.chunk.add_operand(JumpPoint(0), self.state.line);
        self.jumps.insert(stmt.goto, jp_index);

        Ok(())
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result {
        self.visit_expr(&stmt.lhs)?;
        self.visit_expr(&stmt.rhs)?;

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

        Ok(())
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result {
        self.visit_expr(&stmt.from)?;
        self.assigning(|this| this.visit_variable(&stmt.var))?;

        let step_var = self.state.var_gen.next_anonymous();
        let to_var = self.state.var_gen.next_anonymous();

        self.visit_expr(&stmt.to)?;
        self.assigning(|this| this.visit_variable(&to_var))?;

        match stmt.step {
            Some(ref step_expr) => {
                self.visit_expr(step_expr)?;
            }
            None => {
                self.chunk.write_opcode(OpCode::Constant, self.state.line);
                self.chunk.add_operand(1.0, self.state.line);
            }
        }
        self.assigning(|this| this.visit_variable(&step_var))?;

        let for_state = ForState {
            var: stmt.var,
            step: step_var,
            to: to_var,
            start_code_point: self.chunk.len(),
        };

        self.for_states.insert(stmt.var, for_state);

        Ok(())
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result {
        // Do not enforce nesting of for/next pairs, have
        // the runtime deal with it
        let for_state = self
            .for_states
            .remove(&stmt.var)
            .ok_or(CompileError::NextWithoutFor)?;
        let step_var = for_state.step;
        let to_var = for_state.to;
        let loop_start = for_state.start_code_point;

        self.visit_variable(&step_var)?;

        self.chunk.write_opcode(OpCode::Dup, self.state.line);
        self.chunk.write_opcode(OpCode::Sign, self.state.line);
        self.chunk.write_opcode(OpCode::Swap, self.state.line);
        self.visit_variable(&stmt.var)?;
        self.chunk.write_opcode(OpCode::Add, self.state.line);
        self.chunk.write_opcode(OpCode::Dup, self.state.line);

        self.assigning(|this| this.visit_variable(&stmt.var))?;
        self.visit_variable(&to_var)?;
        // value stack: [<step sign>, current, to]
        self.chunk.write_opcode(OpCode::Sub, self.state.line);
        self.chunk.write_opcode(OpCode::Sign, self.state.line);
        self.chunk.write_opcode(OpCode::Sub, self.state.line);
        self.chunk.write_opcode(OpCode::CondJump, self.state.line);
        self.chunk
            .add_operand(JumpPoint(loop_start), self.state.line);

        Ok(())
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result {
        let mut fchunk = Chunk::new();
        let mut fc = FuncCompiler::new(stmt.func, stmt.var, self.state.line, &mut fchunk);
        fc.visit_expr(&stmt.expr)?;
        fchunk.write_opcode(OpCode::Return, self.state.line);

        self.chunk.add_function(stmt.func, fchunk);

        Ok(())
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result {
        for dim in &stmt.dims {
            match dim {
                Either::Left(list) => {
                    self.visit_expr(&list.subscript)?;
                    self.chunk
                        .write_opcode(OpCode::SetArrayBound, self.state.line);
                    self.chunk.add_inline_operand(list.var, self.state.line);
                }
                Either::Right(table) => {
                    self.visit_expr(&table.subscript.0)?;
                    self.visit_expr(&table.subscript.1)?;
                    self.chunk
                        .write_opcode(OpCode::SetArrayBound2d, self.state.line);
                    self.chunk.add_inline_operand(table.var, self.state.line);
                }
            }
        }

        Ok(())
    }

    fn visit_rem(&mut self) -> Result {
        self.chunk.write_opcode(OpCode::Noop, self.state.line);
        Ok(())
    }

    fn visit_end(&mut self) -> Result {
        self.chunk.write_opcode(OpCode::Stop, self.state.line);
        Ok(())
    }

    fn visit_stop(&mut self) -> Result {
        self.chunk.write_opcode(OpCode::Stop, self.state.line);
        Ok(())
    }

    fn visit_return(&mut self) -> Result {
        self.chunk.write_opcode(OpCode::Return, self.state.line);
        Ok(())
    }

    fn visit_variable(&mut self, lval: &Variable) -> Result {
        if self.state.assign {
            self.chunk.write_opcode(OpCode::SetGlobal, self.state.line);
        } else {
            self.chunk.write_opcode(OpCode::GetGlobal, self.state.line);
        }

        self.chunk.add_inline_operand(lval.clone(), self.state.line);

        Ok(())
    }

    fn visit_list(&mut self, list: &List) -> Result {
        self.evaluating(|this| this.visit_expr(&list.subscript))?;
        if self.state.assign {
            self.chunk
                .write_opcode(OpCode::SetGlobalArray, self.state.line);
        } else {
            self.chunk
                .write_opcode(OpCode::GetGlobalArray, self.state.line);
        }
        self.chunk.add_inline_operand(list.var, self.state.line);

        Ok(())
    }

    fn visit_table(&mut self, table: &Table) -> Result {
        self.evaluating(|this| {
            this.visit_expr(&table.subscript.0)
                .and_then(|_| this.visit_expr(&table.subscript.1))
        })?;

        if self.state.assign {
            self.chunk
                .write_opcode(OpCode::SetGlobalArray2d, self.state.line);
        } else {
            self.chunk
                .write_opcode(OpCode::GetGlobalArray2d, self.state.line);
        }
        self.chunk.add_inline_operand(table.var, self.state.line);

        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expression) -> Result {
        if self.state.assign {
            // this in theory should not happen... parser should've taken
            // care of it, if observed, probably a compiler bug
            return Err(CompileError::CannotAssignTo(expr.to_string()));
        }
        match expr {
            Expression::Lit(n) => {
                self.chunk.write_opcode(OpCode::Constant, self.state.line);
                self.chunk.add_operand(*n, self.state.line);
            }
            Expression::Var(v) => {
                self.visit_lvalue(v)?;
            }
            Expression::Call(func, arg) => {
                self.visit_expr(arg)?;
                if func.is_native() {
                    self.chunk.write_opcode(OpCode::CallNative, self.state.line);
                } else {
                    self.chunk.write_opcode(OpCode::Call, self.state.line);
                }
                self.chunk.add_inline_operand(*func, self.state.line);
            }
            Expression::Neg(expr) => {
                self.visit_expr(expr)?;
                self.chunk.write_opcode(OpCode::Negate, self.state.line);
            }
            Expression::Add(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Add, self.state.line);
            }
            Expression::Sub(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Sub, self.state.line);
            }
            Expression::Mul(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Mul, self.state.line);
            }
            Expression::Div(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Div, self.state.line);
            }
            Expression::Pow(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.chunk.write_opcode(OpCode::Pow, self.state.line);
            }
        }

        Ok(())
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
            10 LET X = SIN(10)
            15 DIM X(20, 20), Y(15)
            20 READ X, Y, A
            21 FOR I = 0 TO 14
            22   LET Y(I) = I + 1000
            23 NEXT I
            24 FOR I = 10 TO 1 STEP -1
            25   PRINT Y(I)
            26 NEXT I
            31 DEF FNA(X) = X + 10
            32 DEF FNB(X) = X + FNA(X) + Y
            35 PRINT 99999
            40 PRINT FNB(Y)
            50 REM IGNORE ME
            55 LET X(3, 3) = 99
            60 GOSUB 100
            70 PRINT X(3, 3)
            80 GOTO 800
            100 REM SUBROUTING
            110 PRINT 78787878
            120 PRINT A
            130 RETURN
            150 DATA 10, 20, 30
            800 END"
        );

        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(&mut chunk);

        let result = compiler.visit_program(&ast);
        eprintln!("{:?}", result);

        let mut vm = VM::new(chunk);
        vm.run();

        assert!(false);
    }
}
