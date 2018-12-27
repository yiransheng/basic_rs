use std::collections::VecDeque;
use std::marker::PhantomData;

use basic_rs::ast::{Visitor as AstVisitor, *};
use rustc_hash::FxHashMap;
use slotmap::SlotMap;

use super::builder::IRBuilder;
use super::{
    BinaryOp, Expression as IRExpression, JumpKind, Label,
    Statement as IRStatement,
};

enum CompileError {
    Unsupported,
    Custom(LineNo, &'static str),
}

type CompileResult<T> = Result<T, CompileError>;

struct IRCompiler<Ev> {
    builder: IRBuilder,
    labels: Vec<Label>,
    line_index: usize,
    line_indices: FxHashMap<LineNo, usize>,
    line_no: LineNo,
    for_states: FxHashMap<Variable, ForConf>,
    expr_visitor: PhantomData<Ev>,
}

struct ForConf {
    step: IRExpression,
    target: IRExpression,
    body: Label,
}

impl<Ev> IRCompiler<Ev>
where
    Ev: for<'b> From<&'b mut IRBuilder>
        + AstVisitor<CompileResult<IRExpression>>,
{
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
            .add_branch(JumpKind::JmpZ, loop_cond, self.next_line());
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
        let mut expr_visitor = Ev::from(&mut self.builder);
        expr_visitor.visit_expr(expr)
    }
}

impl<Ev> AstVisitor<Result<(), CompileError>> for IRCompiler<Ev>
where
    Ev: for<'b> From<&'b mut IRBuilder>
        + AstVisitor<CompileResult<IRExpression>>,
{
    fn visit_program(&mut self, prog: &Program) -> Result<(), CompileError> {
        self.init(prog);

        for (i, stmt) in prog.statements.iter().enumerate() {
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
        let mut expr_visitor = Ev::from(&mut self.builder);
        let cond = expr_visitor.visit_if(stmt)?;

        self.builder
            .add_statement(self.current_line(), IRStatement::Logical(cond));

        let from = self.current_line();
        let to = self.lookup_line(stmt.then);

        self.builder.add_branch(JumpKind::JmpNZ, from, to);
        self.builder
            .add_branch(JumpKind::JmpZ, from, self.next_line());

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

/*
 * #[derive(Debug, Copy, Clone)]
 * enum LineRole {
 *     BasicBlock(Label),
 *     BasicBlockEnd(Label),
 *     LoopStart(Label, Label), // header, condition
 *     LoopEnd(Label),
 * }
 *
 * struct ControlFlow {
 *     line_roles: FxHashMap<ast::LineNo, LineRole>,
 *     blocks: SlotMap<Label, Vec<IRStatement>>,
 *     current_line: ast::LineNo,
 *     prev_line: Option<ast::LineNo>,
 * }
 *
 * impl ControlFlow {
 *     fn new_block(&mut self) -> Label {
 *         self.blocks.insert(vec![])
 *     }
 *     fn prev_line_role(&self) -> Option<LineRole> {
 *         self.prev_line.and_then(|line_no| {
 *             self.line_roles.get(line_no)
 *         })
 *     }
 *     fn assgin_block(&mut self) {
 *
 *         if self.line_roles.contains_key(&self.current_line) {
 *             return;
 *         }
 *
 *         let current_line_role = match self.prev_line_role() {
 *             Some(BasicBlock(label)) => {
 *                 LineRole::BasicBlock(label)
 *             }
 *             _ => {
 *                 let label = self.new_block();
 *                 LineRole::BasicBlock(label)
 *             }
 *         };
 *
 *         self.line_roles.insert(self.current_line, current_line_role);
 *     }
 *     fn assign_block_end(&mut self) {
 *         let current_line_role = match self.prev_line_role() {
 *             Some(BasicBlock(label)) => {
 *                 LineRole::BasicBlockEnd(label)
 *             }
 *             _ => {
 *                 let label = self.new_block();
 *                 LineRole::BasicBlockEnd(label)
 *             }
 *         };
 *
 *         self.line_roles.insert(self.current_line, current_line_role);
 *     }
 * }
 *
 * impl trait Visitor<T> for ControlFlow {
 *     fn visit_program(&mut self, prog: &Program) -> T {
 *         for stmt in prog.iter() {
 *             self.current_line = stmt.line_no;
 *             self.visit_statement(stmt);
 *             self.prev_line = Some(stmt.line_no);
 *         }
 *     }
 *
 *     fn visit_statement(&mut self, stmt: &Statement) -> T {
 *         match &stmt.statement {
 *             Stmt::Let(s) => self.visit_let(s),
 *             Stmt::Read(s) => self.visit_read(s),
 *             Stmt::Data(s) => self.visit_data(s),
 *             Stmt::Print(s) => self.visit_print(s),
 *             Stmt::Goto(s) => self.visit_goto(s),
 *             Stmt::Gosub(s) => self.visit_gosub(s),
 *             Stmt::If(s) => self.visit_if(s),
 *             Stmt::For(s) => self.visit_for(s),
 *             Stmt::Next(s) => self.visit_next(s),
 *             Stmt::Def(s) => self.visit_def(s),
 *             Stmt::Dim(s) => self.visit_dim(s),
 *             Stmt::End => self.visit_end(),
 *             Stmt::Rem => self.visit_rem(),
 *             Stmt::Stop => self.visit_stop(),
 *             Stmt::Return => self.visit_return(),
 *         }
 *     }
 *
 *     fn visit_let(&mut self, stmt: &LetStmt) -> T {
 *         self.assign_block();
 *     }
 *
 *     fn visit_read(&mut self, stmt: &ReadStmt) -> T;
 *
 *     fn visit_data(&mut self, stmt: &DataStmt) -> T;
 *
 *     fn visit_print(&mut self, stmt: &PrintStmt) -> T {
 *         self.assign_block();
 *     }
 *
 *     fn visit_goto(&mut self, stmt: &GotoStmt) -> T {
 *         self.assign_block_end();
 *
 *         if self.blocks.contains(stmt.goto) {
 *             return;
 *         }
 *         let label = self.new_block();
 *         self.line_roles.insert(*stmt.goto, LineRole::BasicBlock(label));
 *
 *     }
 *
 *     fn visit_gosub(&mut self, stmt: &GosubStmt) -> T;
 *
 *     fn visit_if(&mut self, stmt: &IfStmt) -> T {
 *         self.assign_block_end();
 *
 *         if self.blocks.contains(stmt.then) {
 *             return;
 *         }
 *         let label = self.new_block();
 *         self.line_roles.insert(*stmt.then, LineRole::BasicBlock(label));
 *     }
 *
 *     fn visit_for(&mut self, stmt: &ForStmt) -> T {
 *         let header_label = self.new_block();
 *         let cond_label = self.new_block();
 *         self.line_roles.insert(*self.current_line, LoopStart(header_label, cond_label));
 *     }
 *
 *     fn visit_next(&mut self, stmt: &NextStmt) -> T {
 *         let prev_line = self.prev_line.unwrap();
 *
 *         if let Some(BasicBlock(label)) = self.prev_line_role()  {
 *             self.line_roles.insert(prev_line, LineRole::BasicBlockEnd(label));
 *         }
 *
 *         let label = self.new_block();
 *         self.line_roles.insert(*self.current_line, LoopEnd(label));
 *     }
 *
 *     fn visit_def(&mut self, stmt: &DefStmt) -> T;
 *
 *     fn visit_dim(&mut self, stmt: &DimStmt) -> T;
 *
 *     fn visit_rem(&mut self) -> T {
 *         self.assign_block();
 *     }
 *
 *     fn visit_end(&mut self) -> T {
 *         self.assign_block_end();
 *     }
 *
 *     fn visit_stop(&mut self) -> T {
 *         self.assign_block_end();
 *     }
 *
 *     fn visit_return(&mut self) -> T;
 *
 *     fn visit_lvalue(&mut self, lval: &LValue) -> T {
 *         match lval {
 *             LValue::Variable(var) => self.visit_variable(var),
 *             LValue::List(list) => self.visit_list(list),
 *             LValue::Table(table) => self.visit_table(table),
 *         }
 *     }
 *
 *     fn visit_variable(&mut self, lval: &Variable) -> T;
 *
 *     fn visit_list(&mut self, list: &List) -> T;
 *
 *     fn visit_table(&mut self, table: &Table) -> T;
 *
 *     fn visit_expr(&mut self, table: &Expression) -> T;
 * }
 */
