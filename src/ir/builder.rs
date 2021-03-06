use crate::ast;
use rustc_hash::{FxHashMap, FxHashSet};
use slotmap::SecondaryMap;

use super::*;

#[derive(Debug)]
pub struct Builder {
    functions: Vec<Function>,
    current_line: ast::LineNo,
    main: Option<FunctionName>,
    vars: FxHashSet<ast::Variable>,
    arrs: FxHashMap<ast::Variable, Offset<()>>,
    fns: FxHashSet<ast::Func>,
    data: Vec<f64>,
    labels: String,
}

impl Builder {
    pub fn new() -> Self {
        Builder {
            functions: vec![],
            current_line: ast::LineNo::default(),
            main: None,
            vars: FxHashSet::default(),
            arrs: FxHashMap::default(),
            fns: FxHashSet::default(),
            data: vec![],
            labels: String::new(),
        }
    }
    pub fn build(mut self) -> Program {
        let mut globals: Vec<_> = self
            .vars
            .iter()
            .map(|var| GlobalKind::Variable(*var))
            .collect();

        globals.extend(
            self.arrs
                .iter()
                .map(|(var, dim)| GlobalKind::ArrPtr(*var, *dim)),
        );
        globals.extend(self.fns.iter().map(|func| GlobalKind::FnPtr(*func)));

        self.data.reverse();

        Program {
            globals,
            functions: self.functions,
            main: self.main.unwrap(),
            data: self.data,
            labels: self.labels,
        }
    }
    pub fn set_line_no(&mut self, line_no: ast::LineNo) {
        self.current_line = line_no;
    }

    pub fn define_global(&mut self, var: ast::Variable) {
        self.vars.insert(var);
    }
    pub fn define_array(
        &mut self,
        var: ast::Variable,
        dim: Offset<()>,
    ) -> Result<(), Offset<()>> {
        if let Some(prev_dim) = self.arrs.insert(var, dim) {
            if prev_dim != dim {
                return Err(prev_dim);
            }
        }

        Ok(())
    }
    pub fn define_function(&mut self, func: ast::Func) {
        self.fns.insert(func);
    }
    pub fn add_data<I: IntoIterator<Item = f64>>(&mut self, data: I) {
        self.data.extend(data);
    }

    pub fn set_main(&mut self, main: FunctionName) -> Result<(), FunctionName> {
        match self.main {
            Some(main) => Err(main),
            _ => {
                self.main = Some(main);
                Ok(())
            }
        }
    }
    pub fn add_function(
        &mut self,
        ty: FnType,
        name: FunctionName,
        entry: Label,
    ) -> Result<(), Function> {
        let mut blocks = SecondaryMap::new();
        blocks.insert(entry, BasicBlock::empty(entry));

        let function = Function {
            name,
            line_no: self.current_line,
            ty,
            locals: vec![],
            entry,
            blocks,
        };

        if self.get_function_mut(name).is_some() {
            Err(function)
        } else {
            self.functions.push(function);
            Ok(())
        }
    }
    pub fn add_local(
        &mut self,
        ty: ValueType,
        name: FunctionName,
    ) -> Result<usize, FunctionName> {
        self.get_function_mut(name)
            .map(|func| {
                let index = func.locals.len();
                func.locals.push(ty);
                index
            })
            .ok_or_else(|| name)
    }
    pub fn add_block(
        &mut self,
        func: FunctionName,
        label: Label,
    ) -> Result<(), Label> {
        let block = BasicBlock::empty(label);

        self.get_function_mut(func)
            .map(|func| match func.blocks.insert(label, block) {
                Some(_) => Err(label),
                None => Ok(()),
            })
            .unwrap_or_else(|| Err(label))
    }
    pub fn add_string_label(&mut self, s: &str) -> (usize, usize) {
        let offset = self.labels.as_bytes().len();
        self.labels.push_str(s);
        (offset, s.as_bytes().len())
    }

    pub fn add_statement(
        &mut self,
        func: FunctionName,
        label: Label,
        statement: Statement,
    ) -> Result<(), Statement> {
        let current_line = self.current_line;
        match self
            .get_function_mut(func)
            .and_then(|func| func.blocks.get_mut(label))
        {
            Some(block) => {
                block.statements.push(statement);
                block.line_nos.push(current_line);
                Ok(())
            }
            _ => Err(statement),
        }
    }

    pub fn add_branch(&mut self, func: FunctionName, from: Label, to: Label) {
        if let Some(block) = self
            .get_function_mut(func)
            .and_then(|func| func.blocks.get_mut(from))
        {
            match block.exit {
                BlockExit::Jump(_) | BlockExit::Return(_) => {
                    block.exit = BlockExit::Jump(to);
                }
                BlockExit::Switch(_, _, ref mut prev_to) => {
                    *prev_to = Some(to);
                }
            }
        }
    }
    pub fn add_conditional_branch(
        &mut self,
        func: FunctionName,
        cond: Expr,
        from: Label,
        true_br: Label,
        false_br: Option<Label>,
    ) {
        if let Some(block) = self
            .get_function_mut(func)
            .and_then(|func| func.blocks.get_mut(from))
        {
            // conditional jump is a actual uncondiational jump, this check
            // is important not only for efficiency, but also without it,
            // binaryen panic!
            if false_br == Some(true_br) {
                block.exit = BlockExit::Jump(true_br)
            } else {
                block.exit = BlockExit::Switch(cond, true_br, false_br)
            }
        }
    }
    pub fn add_return(
        &mut self,
        func: FunctionName,
        label: Label,
        val: Option<Expr>,
    ) {
        if let Some(block) = self
            .get_function_mut(func)
            .and_then(|func| func.blocks.get_mut(label))
        {
            block.exit = BlockExit::Return(val)
        }
    }

    fn get_function_mut(
        &mut self,
        name: FunctionName,
    ) -> Option<&mut Function> {
        self.functions.iter_mut().find(|func| func.name == name)
    }
}
