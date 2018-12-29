use basic_rs::ast;
use rustc_hash::FxHashMap;
use slotmap::{SecondaryMap, SlotMap};

use super::*;

#[derive(Debug)]
pub struct Builder {
    functions: Vec<Function>,
    main: Option<FunctionName>,
}

impl Builder {
    pub fn new() -> Self {
        Builder {
            functions: vec![],
            main: None,
        }
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
        name: FunctionName,
        entry: Label,
    ) -> Result<(), Function> {
        let mut blocks = SecondaryMap::new();
        blocks.insert(entry, BasicBlock::empty(entry));

        let function = Function {
            name,
            local_count: 0,
            entry,
            blocks,
        };

        if let Some(_) = self.get_function_mut(name) {
            Err(function)
        } else {
            self.functions.push(function);
            Ok(())
        }
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

    pub fn add_statement(
        &mut self,
        func: FunctionName,
        label: Label,
        statement: Statement,
    ) -> Result<(), Statement> {
        match self
            .get_function_mut(func)
            .and_then(|func| func.blocks.get_mut(label))
        {
            Some(block) => {
                block.statements.push(statement);
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
            block.exit = BlockExit::Switch(cond, true_br, false_br)
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
