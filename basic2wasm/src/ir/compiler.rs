use std::collections::VecDeque;
use std::marker::PhantomData;

use basic_rs::ast::{Visitor as AstVisitor, *};
use rustc_hash::FxHashMap;
use slotmap::SlotMap;

use super::builder::IRBuilder;
use super::expr_compiler::ExprCompiler;
use super::{
    BinaryOp, Expression as IRExpression, JumpKind, Label,
    Statement as IRStatement, IR,
};

#[derive(Debug)]
pub enum CompileError {
    Unsupported,
    Custom(LineNo, &'static str),
}

type CompileResult<T> = Result<T, CompileError>;

pub struct IRCompiler {
    builder: IRBuilder,

    labels: Vec<Label>,
    line_index: usize,
    line_indices: FxHashMap<LineNo, usize>,
    line_no: LineNo,
    for_states: FxHashMap<Variable, ForConf>,
}

struct ForConf {
    step: IRExpression,
    target: IRExpression,
    body: Label,
}

impl IRCompiler {
    pub fn new() -> Self {
        IRCompiler {
            builder: IRBuilder::new(),
            labels: vec![],
            line_index: 0,
            line_indices: FxHashMap::default(),
            line_no: 0,
            for_states: FxHashMap::default(),
        }
    }
    pub fn compile(mut self, program: &Program) -> CompileResult<IR> {
        self.visit_program(program)?;
        let ir = self.builder.build();

        Ok(ir)
    }

    fn init<'a>(&'a mut self, program: &'a Program) {
        let marker = LineMarker {
            builder: &mut self.builder,
            program,
        };

        self.labels = marker.mark();

        let end_label = self.builder.create_block();
        self.labels.push(end_label);

        debug_assert!(self.labels.len() == program.statements.len() + 1);

        // TODO: no need to track every line, use a scan
        // (also switch to BTreeMap?)
        self.line_indices = program
            .statements
            .iter()
            .enumerate()
            .map(|(i, stmt)| (stmt.line_no, i))
            .collect();
    }

    fn current_line(&self) -> Label {
        self.labels[self.line_index]
    }
    fn current_line_no(&self) -> LineNo {
        self.line_no
    }
    fn next_line(&self) -> Label {
        self.labels[self.line_index + 1]
    }
    fn lookup_line(&self, line_no: LineNo) -> Label {
        let line_index = *self.line_indices.get(&line_no).unwrap();
        self.labels[line_index]
    }

    fn branch_to_next_line(&mut self) {
        let from = self.current_line();
        let to = self.next_line();

        if from != to {
            self.builder.add_branch(JumpKind::Jmp, from, to);
        }
    }

    fn add_for(
        &mut self,
        var: Variable,
        step: IRExpression,
        target: IRExpression,
    ) {
        let sym_index = self.builder.sym_global(var);
        let sym_step = self.builder.sym_local();
        let sym_target = self.builder.sym_local();

        self.builder.add_statement(
            self.current_line(),
            IRStatement::Assign(sym_step, step),
        );
        self.builder.add_statement(
            self.current_line(),
            IRStatement::Assign(sym_target, target),
        );

        let step = IRExpression::Get(sym_step);
        let target = IRExpression::Get(sym_target);

        let loop_cond = self.builder.create_block();

        self.builder.add_statement(
            loop_cond,
            IRStatement::Logical(IRExpression::LoopCondition(Box::new([
                IRExpression::Get(sym_step),
                IRExpression::Get(sym_target),
                IRExpression::Get(sym_index),
            ]))),
        );

        self.for_states.insert(
            var,
            ForConf {
                step,
                target,
                body: loop_cond,
            },
        );

        self.builder
            .add_branch(JumpKind::Jmp, self.current_line(), loop_cond);

        self.builder
            .add_branch(JumpKind::Jmp, loop_cond, self.next_line());
    }

    fn pop_for(&mut self, var: Variable) -> CompileResult<ForConf> {
        self.for_states.remove(&var).ok_or_else(|| {
            CompileError::Custom(self.current_line_no(), "Next without for")
        })
    }

    fn compile_expr(
        &mut self,
        expr: &Expression,
    ) -> CompileResult<IRExpression> {
        let mut expr_visitor = ExprCompiler::from(&mut self.builder);
        expr_visitor.visit_expr(expr)
    }

    fn compile_if(&mut self, expr: &IfStmt) -> CompileResult<IRExpression> {
        let mut expr_visitor = ExprCompiler::from(&mut self.builder);
        expr_visitor.visit_if(expr)
    }
}

impl AstVisitor<Result<(), CompileError>> for IRCompiler {
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        self.init(prog);

        for (i, stmt) in prog.statements.iter().enumerate() {
            if i == 0 {
                self.builder.set_entry_block(self.labels[0]);
            }
            self.line_index = i;
            self.line_no = stmt.line_no;
            self.visit_statement(stmt)?;
        }

        Ok(())
    }

    fn visit_let(&mut self, stmt: &LetStmt) -> Result<(), CompileError> {
        let var = match stmt.var {
            LValue::Variable(var) => var,
            _ => return Err(CompileError::Unsupported),
        };
        let expr = self.compile_expr(&stmt.expr)?;
        let symbol = self.builder.sym_global(var);

        self.builder.add_statement(
            self.current_line(),
            IRStatement::Assign(symbol, expr),
        );

        self.branch_to_next_line();

        Ok(())
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result<(), CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_data(&mut self, stmt: &DataStmt) -> Result<(), CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result<(), CompileError> {
        for part in &stmt.parts {
            match part {
                Printable::Expr(expr) => {
                    let expr = self.compile_expr(expr)?;
                    self.builder.add_statement(
                        self.current_line(),
                        IRStatement::Print(expr),
                    );
                }
                _ => {}
            }
        }

        self.branch_to_next_line();

        Ok(())
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result<(), CompileError> {
        let from = self.current_line();
        let to = self.lookup_line(stmt.goto);

        self.builder.add_branch(JumpKind::Jmp, from, to);

        Ok(())
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result<(), CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result<(), CompileError> {
        let cond = self.compile_if(stmt)?;

        self.builder
            .add_statement(self.current_line(), IRStatement::Logical(cond));

        let from = self.current_line();
        let to = self.lookup_line(stmt.then);

        self.builder.add_branch(JumpKind::JmpNZ, from, to);
        self.builder
            .add_branch(JumpKind::Jmp, from, self.next_line());

        Ok(())
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result<(), CompileError> {
        let step = match stmt.step {
            Some(ref expr) => self.compile_expr(expr)?,
            _ => IRExpression::Const(1.0),
        };
        let target = self.compile_expr(&stmt.to)?;
        self.add_for(stmt.var, step, target);

        Ok(())
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result<(), CompileError> {
        let ForConf { step, body, .. } = self.pop_for(stmt.var)?;

        let sym = self.builder.sym_global(stmt.var);
        let expr = IRExpression::Binary(
            BinaryOp::Add,
            Box::new(IRExpression::Get(sym)),
            Box::new(step),
        );

        self.builder
            .add_statement(self.current_line(), IRStatement::Assign(sym, expr));
        self.builder
            .add_branch(JumpKind::Jmp, self.current_line(), body);

        self.builder
            .add_branch(JumpKind::JmpNZ, body, self.next_line());

        Ok(())
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result<(), CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result<(), CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_rem(&mut self) -> Result<(), CompileError> {
        self.branch_to_next_line();

        Ok(())
    }

    fn visit_end(&mut self) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_stop(&mut self) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_return(&mut self) -> Result<(), CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_variable(&mut self, lval: &Variable) -> Result<(), CompileError> {
        Ok(())
    }

    fn visit_list(&mut self, list: &List) -> Result<(), CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_table(&mut self, table: &Table) -> Result<(), CompileError> {
        Err(CompileError::Unsupported)
    }

    fn visit_expr(&mut self, table: &Expression) -> Result<(), CompileError> {
        Ok(())
    }
}

struct LineMarker<'a> {
    builder: &'a mut IRBuilder,
    program: &'a Program,
}

impl<'a> LineMarker<'a> {
    fn mark(self) -> Vec<Label> {
        let statements = &self.program.statements;
        let n = statements.len();

        if n == 0 {
            return vec![];
        }

        let unreachable_label = self.builder.create_block();
        let entry_label = self.builder.create_block();

        // NOTE: unreachable_label is just another block label
        // defer to binaryen for deadcode elimination
        let mut labels: Vec<Label> = vec![unreachable_label; n];

        // Using a closure fails borrow-chk, workaround with a macro -_-
        macro_rules! visited {
            ($i: expr) => {
                if $i < n {
                    labels[$i] != unreachable_label
                } else {
                    true
                }
            };
        }

        let mut stack: VecDeque<usize> = VecDeque::new();

        for (i, stmt) in statements.iter().enumerate() {
            match &stmt.statement {
                Stmt::Goto(stmt) => {
                    let to_index = self.find_line_index(&stmt.goto).unwrap();
                    if !visited!(to_index) {
                        labels[to_index] = self.builder.create_block();
                    }
                }
                // not-yet
                Stmt::Gosub(stmt) => {
                    let to_index = self.find_line_index(&stmt.goto).unwrap();
                    if !visited!(to_index) {
                        labels[to_index] = self.builder.create_block();
                    }
                }
                Stmt::If(stmt) => {
                    let to_index = self.find_line_index(&stmt.then).unwrap();
                    if !visited!(to_index) {
                        labels[to_index] = self.builder.create_block();
                    }
                }
                _ => {
                    if !visited!(i) {
                        stack.push_front(i);
                    }
                }
            }
        }

        labels[0] = entry_label;

        loop {
            if stack.is_empty() {
                return labels;
            }
            let index = stack.pop_back().unwrap();
            let next_line_index = index + 1;

            let current_label = labels[index];
            let stmt = &statements[index];

            match &stmt.statement {
                Stmt::End | Stmt::Stop => {}
                // non-control-flow statements
                Stmt::Let(_)
                | Stmt::Read(_)
                | Stmt::Data(_)
                | Stmt::Print(_)
                | Stmt::Def(_)
                | Stmt::Dim(_)
                | Stmt::Rem => {
                    if !visited!(next_line_index) {
                        labels[next_line_index] = current_label;
                        stack.push_back(next_line_index);
                    }
                }
                // conditional branch
                Stmt::If(stmt) => {
                    //TODO: Error?
                    let to_index = self.find_line_index(&stmt.then).unwrap();

                    if !visited!(to_index) {
                        let new_label = self.builder.create_block();
                        labels[to_index] = new_label;
                        stack.push_back(to_index);
                    }

                    if !visited!(next_line_index) {
                        let new_label = self.builder.create_block();
                        labels[next_line_index] = new_label;
                        stack.push_back(next_line_index);
                    }
                }
                // unconditional branch
                Stmt::Goto(stmt) => {
                    //TODO: Error
                    let to_index = self.find_line_index(&stmt.goto).unwrap();
                    if !visited!(to_index) {
                        let new_label = self.builder.create_block();
                        labels[to_index] = new_label;
                        stack.push_back(to_index);
                    }
                }
                Stmt::Next(_) | Stmt::For(_) => {
                    let new_label = self.builder.create_block();
                    labels[index] = new_label;

                    if !visited!(next_line_index) {
                        let new_label = self.builder.create_block();
                        labels[next_line_index] = new_label;
                        stack.push_back(next_line_index);
                    }
                }
                _ => unimplemented!(),
            }
        }
    }

    #[inline(always)]
    fn find_line_index(&self, line_no: &LineNo) -> Option<usize> {
        find_line_index(&self.program, line_no)
    }
}

//TODO: this could be cached
fn find_line_index(program: &Program, line_no: &LineNo) -> Option<usize> {
    program
        .statements
        .binary_search_by_key(line_no, |s| s.line_no)
        .ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use basic_rs::{Parser, Scanner};
    use indoc::*;

    #[test]
    fn test_line_marker_no_jumps() {
        let program = indoc!(
            "
            10 REM Comment
            20 LET X = 10
            30 LET Y = X + 1
            40 DATA 3 
            40 READ D
            99 END"
        );

        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();
        let mut builder = IRBuilder::new();

        let marker = LineMarker {
            builder: &mut builder,
            program: &ast,
        };
        let labels = marker.mark();
        let n = labels.len();

        for (i, label) in labels.iter().enumerate() {
            let next_index = i + 1;
            if next_index < n {
                assert_eq!(label, &labels[next_index]);
            }
        }
    }

    #[test]
    fn test_line_marker_simple() {
        let program = indoc!(
            "
            10 REM Comment
            20 LET X = 10
            30 IF X > 0 THEN 50
            40 PRINT X
            50 FOR I = 1 TO 10
            60 REM loop body
            70 NEXT I
            80 GOTO 20
            99 END"
        );

        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();
        let mut builder = IRBuilder::new();

        let marker = LineMarker {
            builder: &mut builder,
            program: &ast,
        };
        let labels = marker.mark();

        println!("{:?}", labels);

        // line 20 has new label due to GOTO in 80
        assert_ne!(labels[0], labels[1]); // 10-20

        assert_eq!(labels[1], labels[2]); // 20-30

        assert_ne!(labels[2], labels[3]); // 30-40
        assert_ne!(labels[3], labels[4]); // 40-50
        assert_ne!(labels[4], labels[5]); // 50-60
        assert_ne!(labels[5], labels[6]); // 60-70
        assert_ne!(labels[6], labels[7]); // 70-80
        assert_ne!(labels[7], labels[8]); // 80-90
    }
}
