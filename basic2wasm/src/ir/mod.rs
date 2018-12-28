mod builder;
mod codegen;
mod compiler;

use basic_rs::ast;
use slotmap::{new_key_type, SecondaryMap, SlotMap};
use smallvec::SmallVec;

new_key_type! { pub struct Symbol; }
new_key_type! { pub struct Label; }

#[derive(Debug, Copy, Clone)]
pub enum SymbolKind {
    Global(ast::Variable),
    Local(usize),
    // Function(usize)
    // Subroutine(usize)
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Neg,
    EqZ, // Not
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    Greater,
    Equal,
}

#[derive(Debug)]
pub enum Expression {
    Const(f64),
    Get(Symbol),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    // step, target, current,
    LoopCondition(Box<[Expression; 3]>),
    // Call(Symbol, Box<Expression>)
}

#[derive(Debug)]
pub enum Statement {
    Assign(Symbol, Expression),
    Logical(Expression),
    Print(Expression),
}

#[derive(Debug, Copy, Clone)]
pub enum JumpKind {
    Jmp,
    JmpZ,
    JmpNZ,
}

#[derive(Debug)]
pub struct Branches {
    to: SmallVec<[(JumpKind, Label); 2]>,
}
impl Branches {
    fn new() -> Self {
        Branches {
            to: SmallVec::new(),
        }
    }
    fn add_branch(&mut self, j_kind: JumpKind, to: Label) {
        self.to.push((j_kind, to));
    }
    fn iter(&self) -> impl Iterator<Item = &(JumpKind, Label)> {
        self.to.iter()
    }
}

#[derive(Debug)]
pub struct IR {
    entry_block: Label,
    symbols: SlotMap<Symbol, SymbolKind>,
    blocks: SlotMap<Label, ()>,
    code: SecondaryMap<Label, Vec<Statement>>,
    branches: SecondaryMap<Label, Branches>,
}

impl IR {
    pub fn symbol_kind(&self, sym: Symbol) -> SymbolKind {
        *self.symbols.get(sym).unwrap()
    }
}
