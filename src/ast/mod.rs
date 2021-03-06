pub mod function;
pub mod keyword;
pub mod variable;

mod display;
mod token;
mod visitor;

use std::fmt;

pub use self::token::*;
pub use self::variable::*;
pub use self::visitor::Visitor;

pub use self::function::Func;

pub type Number = f64;

#[derive(Debug, Copy, Clone, Default, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct LineNo(pub usize);

impl Into<usize> for LineNo {
    fn into(self) -> usize {
        self.0
    }
}
impl fmt::Display for LineNo {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

use crate::scanner::SourceLoc;
use either::Either;

#[derive(Debug)]
pub struct List {
    pub var: Variable,
    pub subscript: Expression,
}

#[derive(Debug)]
pub struct Table {
    pub var: Variable,
    pub subscript: (Expression, Expression),
}

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
pub enum LValue {
    Variable(Variable),
    List(List),
    Table(Table),
}

#[derive(Debug)]
pub enum Expression {
    Lit(Number),
    Var(Box<LValue>),
    Neg(Box<Expression>),
    Call(Func, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Rem(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum Printable {
    Label(String),
    Expr(Expression),
    Advance3,  // ;
    Advance15, // ,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Statement {
    pub statement: Stmt,
    pub line_no: LineNo, // BASIC line number
    pub loc: SourceLoc,
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
    Input(InputStmt),
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
    pub var: LValue,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct ReadStmt {
    pub vars: Vec<LValue>,
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
    pub func: Func,
    pub var: Variable,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct DimStmt {
    pub dims: Vec<Either<List, Table>>,
}

#[derive(Debug)]
pub struct InputStmt {
    pub prompts: Vec<Printable>,
    pub vars: Vec<LValue>,
}
