pub mod function;
pub mod keyword;

mod token;

pub use self::token::*;

use self::function::Func;

pub type Number = f64;
pub type LineNo = usize;

#[derive(Debug)]
pub struct Variable(pub [u8; 2]);

#[derive(Debug)]
pub struct List(pub [u8; 2]);

#[derive(Debug)]
pub enum Relop {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub enum Primary {
    Num(Number),
    Var(Variable),
}

#[derive(Debug)]
pub enum Expression {
    Val(Primary),
    Neg(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
    Call(Func, Box<Expression>),
}

#[derive(Debug)]
pub enum Printable {
    Label(String),
    Expr(Expression),
    Advance3, // ;
    Advance5, // ,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Statement {
    pub statement: Stmt,
    pub line_no: LineNo,
}

#[derive(Debug)]
pub enum Stmt {
    Let(LetStmt),
    Read(ReadStmt),
    Data(DataStmt),
    Print(PrintStmt),
    Goto(GotoStmt),
    Gosub(GosubStmt),
    If(IfStmt),
    For(ForStmt),
    Next(NextStmt),
    Def(DefStmt),
    Dim(DimStmt),
    Rem,
    End,
    Stop,
    Return,
}

#[derive(Debug)]
pub struct LetStmt {
    pub var: Variable,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct ReadStmt {
    pub vars: Vec<Variable>,
}

#[derive(Debug)]
pub struct DataStmt {
    pub vals: Vec<Number>,
}

#[derive(Debug)]
pub struct PrintStmt {
    pub parts: Vec<Printable>,
}

#[derive(Debug)]
pub struct GotoStmt {
    pub goto: LineNo,
}

#[derive(Debug)]
pub struct GosubStmt {
    pub goto: LineNo,
}

#[derive(Debug)]
pub struct IfStmt {
    pub op: Relop,
    pub lhs: Expression,
    pub rhs: Expression,
    pub then: LineNo,
}

#[derive(Debug)]
pub struct ForStmt {
    pub var: Variable,
    pub from: Expression,
    pub to: Expression,
    pub step: Option<Expression>,
}

#[derive(Debug)]
pub struct NextStmt {
    pub var: Variable,
}

#[derive(Debug)]
pub struct DefStmt {
    pub func_id: u8, // 0 - 25 or A-Z
    pub var: Variable,
    pub expr: Expression,
}

// TODO
#[derive(Debug)]
pub struct DimStmt {}
