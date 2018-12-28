use basic_rs::ast;
use rustc_hash::FxHashMap;
use slotmap::{SecondaryMap, SlotMap};

use super::*;

pub struct IRBuilder {
    entry_block: Option<Label>,
    blocks: SlotMap<Label, ()>,
    code: SecondaryMap<Label, Vec<Statement>>,
    branches: SecondaryMap<Label, Branches>,

    symbols: SlotMap<Symbol, SymbolKind>,
    variables: FxHashMap<ast::Variable, Symbol>,
    local_counter: usize,
}

impl IRBuilder {
    pub fn new() -> Self {
        IRBuilder {
            entry_block: None,
            blocks: SlotMap::with_key(),
            code: SecondaryMap::new(),
            branches: SecondaryMap::new(),

            symbols: SlotMap::with_key(),
            variables: FxHashMap::default(),
            local_counter: 0,
        }
    }
    pub fn build(self) -> IR {
        let IRBuilder {
            blocks,
            symbols,
            code,
            branches,
            entry_block,
            ..
        } = self;

        IR {
            entry_block: entry_block.unwrap(),
            symbols,
            blocks,
            code,
            branches,
        }
    }

    pub fn set_entry_block(&mut self, label: Label) {
        self.entry_block = Some(label);
    }

    pub fn create_block(&mut self) -> Label {
        let label = self.blocks.insert(());

        self.code.insert(label, Vec::new());
        self.branches.insert(label, Branches::new());

        label
    }
    pub fn sym_array(&mut self, var: ast::Variable) -> Symbol {
        unimplemented!()
    }
    pub fn sym_global(&mut self, var: ast::Variable) -> Symbol {
        match self.variables.get(&var) {
            Some(sym) => *sym,
            _ => {
                let sym = self.symbols.insert(SymbolKind::Global(var));
                self.variables.insert(var, sym);
                sym
            }
        }
    }
    pub fn sym_local(&mut self) -> Symbol {
        let sym = self.symbols.insert(SymbolKind::Local(self.local_counter));
        self.local_counter += 1;
        sym
    }
    pub fn add_statement(&mut self, label: Label, statement: Statement) {
        self.code.get_mut(label).unwrap().push(statement);
    }

    pub fn add_branch(&mut self, j_kind: JumpKind, from: Label, to: Label) {
        let branches = self.branches.get_mut(from).unwrap();
        branches.add_branch(j_kind, to);
    }
}
