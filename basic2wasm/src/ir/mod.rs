mod builder;
// mod codegen;
// mod compiler;
// mod expr_compiler;

pub use self::builder::Builder;

use basic_rs::ast;
use binaryen::Module;
use slotmap::{new_key_type, SecondaryMap, SlotMap};

new_key_type! { pub struct Label; }
new_key_type! { pub struct FunctionName; }

#[derive(Debug, Copy, Clone)]
pub enum GlobalKind {
    Variable(ast::Variable),
    ArrPtr(ast::Variable),
    FnPtr(ast::Func),
}

#[derive(Debug)]
pub struct Program {
    globals: Vec<GlobalKind>,
    functions: Vec<Function>,
    main: FunctionName,
}

#[derive(Debug)]
pub struct Function {
    name: FunctionName,
    local_count: usize,
    entry: Label,
    blocks: SecondaryMap<Label, BasicBlock>,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub label: Label,
    pub statements: Vec<Statement>,
    pub exit: BlockExit,
}

impl BasicBlock {
    pub fn empty(label: Label) -> Self {
        BasicBlock {
            label,
            statements: vec![],
            exit: BlockExit::Return(None),
        }
    }
}

#[derive(Debug)]
pub enum BlockExit {
    Return(Option<Expr>),
    Jump(Label),
    // cond, true branch, false branch
    Switch(Expr, Label, Option<Label>),
}

#[derive(Debug)]
pub enum LValue {
    ArrPtr(ast::Variable, Offset),
    FnPtr(ast::Func),
    Global(ast::Variable),
    Local(usize),
}

#[derive(Debug)]
pub enum Offset {
    OneD(Expr),
    TwoD(Expr, Expr),
}

#[derive(Debug)]
pub enum Expr {
    RandF64,
    Const(f64),
    Get(Box<LValue>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Call(Box<LValue>, Box<Expr>),
}

#[derive(Debug)]
pub enum Statement {
    Assign(LValue, Expr),
    DefFn(LValue, FunctionName),
    CallSub(FunctionName),
    Alloc1d(LValue, Expr),
    Alloc2d(LValue, Expr, Expr),
    Print(Expr),
    PrintLabel(String),
    PrintAdvance3,
    PrintAdvance15,
    PrintNewline,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Not,
    Neg,
    Sin,
    Cos,
    Atn,
    Exp,
    Abs,
    Log,
    Sqr,
    Trunc,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Less,
    Greater,
    CopySign,
    Equal,
    And,
    Or,
}
