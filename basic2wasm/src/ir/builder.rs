use basic_rs::ast;
use rustc_hash::FxHashMap;
use slotmap::SlotMap;

use super::*;

pub struct IRBuilder {
    blocks: SlotMap<Label, usize>,
    // symbols: SlotMap<Symbol, SymbolKind>,

    // variables: FxHashMap<ast::Variable, Symbol>,
}

impl IRBuilder {
    pub fn new() -> Self {
        IRBuilder {
            blocks: SlotMap::with_key(),
        }
    }

    pub fn create_block(&mut self) -> Label {
        self.blocks.insert(0)
    }
    pub fn sym_global(&mut self, var: ast::Variable) -> Symbol {
        unimplemented!()
    }
    pub fn sym_local(&mut self) -> Symbol {
        unimplemented!()
    }
    pub fn add_statement(&mut self, label: Label, statement: Statement) {}

    pub fn add_branch(&mut self, j_kind: JumpKind, from: Label, to: Label) {}
}
