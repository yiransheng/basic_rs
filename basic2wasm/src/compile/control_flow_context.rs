use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::VecDeque;

use basic_rs::ast::*;
use slotmap::{SecondaryMap, SlotMap};

use crate::ir::{FunctionName, Label};

struct LineCtx {
    line_no: LineNo,
    label: Option<Label>,
    func: Option<FunctionName>,
}

pub struct CfCtx {
    lines: Vec<LineCtx>,
    functions: SecondaryMap<FunctionName, usize>,
    index_cache: RefCell<BTreeMap<LineNo, usize>>,
    labels: SlotMap<Label, ()>,
}

#[derive(Debug)]
pub enum CfError {
    MissingLine(LineNo),
    JumpInsideSubroutine(LineNo),
    UnreachableCode(LineNo),
}

impl CfCtx {
    pub fn from_program(program: &Program) -> Result<Self, CfError> {
        let mut cf_ctx = Self::empty_from_program(program);
        let mut func_gen: SlotMap<FunctionName, ()> = SlotMap::with_key();

        let statements = &program.statements;
        let n = statements.len();

        if n == 0 {
            return Ok(cf_ctx);
        }

        macro_rules! labeled {
            ($i: expr) => {
                if $i < n {
                    let line_ctx = &cf_ctx.lines[$i];
                    line_ctx.label.is_some()
                } else {
                    true
                }
            };
        }
        macro_rules! visited {
            ($i: expr) => {
                if $i < n {
                    let line_ctx = &cf_ctx.lines[$i];
                    line_ctx.label.is_some() && line_ctx.func.is_some()
                } else {
                    true
                }
            };
        }

        let mut stack: VecDeque<usize> = VecDeque::new();

        for (i, stmt) in statements.iter().enumerate() {
            match &stmt.statement {
                Stmt::Goto(stmt) => {
                    let to_index = cf_ctx
                        .find_line_index(stmt.goto)
                        .ok_or_else(|| CfError::MissingLine(stmt.goto))?;

                    if !labeled!(to_index) {
                        let label = cf_ctx.add_label();
                        cf_ctx.set_label(to_index, label);
                    }
                }
                Stmt::Gosub(stmt) => {
                    let to_index = cf_ctx
                        .find_line_index(stmt.goto)
                        .ok_or_else(|| CfError::MissingLine(stmt.goto))?;

                    if !labeled!(to_index) {
                        let label = cf_ctx.add_label();
                        let func = func_gen.insert(());

                        cf_ctx.functions.insert(func, to_index);

                        cf_ctx.set_label(to_index, label);
                        let _ = cf_ctx.set_func(to_index, func);

                        stack.push_front(to_index);
                    }
                }
                Stmt::Def(_) => {
                    let func = func_gen.insert(());

                    cf_ctx.functions.insert(func, i);
                }
                Stmt::If(stmt) => {
                    let to_index = cf_ctx
                        .find_line_index(stmt.then)
                        .ok_or_else(|| CfError::MissingLine(stmt.then))?;

                    if !labeled!(to_index) {
                        let label = cf_ctx.add_label();
                        cf_ctx.set_label(to_index, label);
                    }
                }
                Stmt::Data(..) => {}
                _ => {}
            }
        }

        let entry_label = cf_ctx.add_label();
        cf_ctx.set_label(0, entry_label);

        let main_func = func_gen.insert(());
        cf_ctx.set_func(0, main_func)?;
        cf_ctx.functions.insert(main_func, 0);

        stack.push_back(0);

        loop {
            if stack.is_empty() {
                break;
            }

            let index = stack.pop_back().unwrap();
            let next_line_index = index + 1;

            let current_label = cf_ctx.get_label(index).ok_or_else(|| {
                let line_no = cf_ctx.find_line_no(index);
                CfError::UnreachableCode(line_no)
            })?;
            let current_func = cf_ctx.get_func(index).ok_or_else(|| {
                let line_no = cf_ctx.find_line_no(index);
                CfError::UnreachableCode(line_no)
            })?;

            let stmt = &statements[index];

            match &stmt.statement {
                Stmt::End | Stmt::Stop | Stmt::Return => {}
                // non-control-flow statements
                Stmt::Gosub(_)
                | Stmt::Let(_)
                | Stmt::Read(_)
                | Stmt::Data(_)
                | Stmt::Print(_)
                | Stmt::Def(_)
                | Stmt::Dim(_)
                | Stmt::Rem => {
                    if !visited!(next_line_index) {
                        cf_ctx.set_label(next_line_index, current_label);
                        cf_ctx.set_func(next_line_index, current_func)?;

                        stack.push_back(next_line_index);
                    }
                }
                // conditional branch
                Stmt::If(stmt) => {
                    let to_index = cf_ctx.find_line_index(stmt.then).unwrap();

                    if !visited!(to_index) {
                        let new_label = cf_ctx.add_label();

                        cf_ctx.set_label(to_index, new_label);
                        cf_ctx.set_func(to_index, current_func)?;

                        stack.push_back(to_index);
                    }

                    if !visited!(next_line_index) {
                        let new_label = cf_ctx.add_label();

                        cf_ctx.set_label(next_line_index, new_label);
                        cf_ctx.set_func(next_line_index, current_func)?;

                        stack.push_back(next_line_index);
                    }
                }
                // unconditional branch
                Stmt::Goto(stmt) => {
                    let to_index = cf_ctx.find_line_index(stmt.goto).unwrap();

                    if !visited!(to_index) {
                        let new_label = cf_ctx.add_label();

                        cf_ctx.set_label(to_index, new_label);
                        cf_ctx.set_func(to_index, current_func)?;

                        stack.push_back(to_index);
                    }
                }
                Stmt::Next(_) | Stmt::For(_) => {
                    let new_label = cf_ctx.add_label();
                    cf_ctx.set_label(index, new_label);

                    if !visited!(next_line_index) {
                        let new_label = cf_ctx.add_label();

                        cf_ctx.set_label(next_line_index, new_label);
                        cf_ctx.set_func(next_line_index, current_func)?;

                        stack.push_back(next_line_index);
                    }
                }
            }
        }

        Ok(cf_ctx)
    }

    pub fn find_line_no(&self, line_index: usize) -> LineNo {
        self.lines[line_index].line_no
    }

    pub fn find_line_index(&self, line_no: LineNo) -> Option<usize> {
        if let Some(line_index) = self.index_cache.borrow().get(&line_no) {
            return Some(*line_index);
        }

        let line_index = self
            .lines
            .binary_search_by_key(&line_no, |s| s.line_no)
            .ok();

        if let Some(line_index) = line_index {
            self.index_cache.borrow_mut().insert(line_no, line_index);
        }

        line_index
    }

    pub fn add_label(&mut self) -> Label {
        self.labels.insert(())
    }

    pub fn functions<'a>(
        &'a self,
    ) -> impl Iterator<Item = (FunctionName, Label)> + 'a {
        let lines = &self.lines;

        self.functions
            .iter()
            .filter_map(move |(k, i)| match lines[*i].label {
                Some(label) => Some((k, label)),
                _ => None,
            })
    }

    pub fn get_def_func(&self, index: usize) -> Option<FunctionName> {
        self.functions
            .iter()
            .find(|(name, i)| **i == index)
            .map(|(name, _)| name)
    }

    pub fn get_label(&self, index: usize) -> Option<Label> {
        self.lines.get(index).and_then(|x| x.label)
    }

    pub fn get_func(&self, index: usize) -> Option<FunctionName> {
        self.lines.get(index).and_then(|x| x.func)
    }

    fn empty_from_program(program: &Program) -> Self {
        let lines: Vec<_> = program
            .statements
            .iter()
            .map(|s| LineCtx {
                line_no: s.line_no,
                label: None,
                func: None,
            })
            .collect();
        let index_cache = RefCell::new(BTreeMap::default());

        CfCtx {
            lines,
            index_cache,
            labels: SlotMap::with_key(),
            functions: SecondaryMap::new(),
        }
    }

    fn set_label(&mut self, index: usize, label: Label) -> Option<Label> {
        let prev = self.lines[index].label;
        self.lines[index].label = Some(label);

        prev
    }

    fn set_func(
        &mut self,
        index: usize,
        func: FunctionName,
    ) -> Result<(), CfError> {
        let prev = self.lines[index].func;

        match prev {
            Some(prev_func) if prev_func != func => {
                let line_no = self.lines[index].line_no;
                Err(CfError::JumpInsideSubroutine(line_no))
            }
            _ => {
                self.lines[index].func = Some(func);
                Ok(())
            }
        }
    }
}