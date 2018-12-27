mod builder;
mod control_flow;

use basic_rs::ast;
use slotmap::{new_key_type, SlotMap};

new_key_type! { pub struct Symbol; }
new_key_type! { pub struct Label; }

#[derive(Debug, Copy, Clone)]
pub enum SymbolKind {
    Global(ast::Variable),
    Local(usize),
    // Function(usize)
    // Subroutine(usize)
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    EqZ, // Not
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum JumpKind {
    Jmp,
    JmpZ,
    JmpNZ,
}

#[derive(Debug)]
pub struct Jump {
    pub from: Label,
    pub to: Label,
    pub kind: JumpKind,
}

#[derive(Debug)]
pub struct IR {
    symbols: SlotMap<Symbol, SymbolKind>,
    blocks: SlotMap<Label, usize>,
    jumps: Vec<Jump>,
}
