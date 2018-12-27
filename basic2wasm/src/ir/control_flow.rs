use std::collections::VecDeque;

use basic_rs::ast::*;
use rustc_hash::FxHashMap;
use slotmap::SlotMap;

use super::builder::IRBuilder;
use super::{Label, Statement as IRStatement};

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

        // NOTE: unreachable_label is just another block
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
