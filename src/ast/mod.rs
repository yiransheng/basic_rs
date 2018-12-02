pub mod function;
pub mod keyword;

mod token;

pub use self::token::*;

pub type Number = f64;
pub type LineNo = usize;

pub struct Variable([u8; 2]);

pub enum Relop {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}

pub enum Expression {
    Num(Number),
    Var(Variable),
    Neg(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
}

pub enum Printable {
    Label(String),
    Expr(Expression),
    Advance3, // ;
    Advance5, // ,
}

pub struct Program {
    statements: Vec<(LineNo, Statement)>,
}

pub enum Statement {
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
    End,
    Stop,
    Return,
}

pub struct LetStmt {
    var: Variable,
    expr: Expression,
}

pub struct ReadStmt {
    vars: Vec<Variable>,
}

pub struct DataStmt {
    vals: Vec<Number>,
}

pub struct PrintStmt {
    parts: Vec<Printable>,
}

pub struct GotoStmt {
    goto: LineNo,
}
pub struct GosubStmt {
    goto: LineNo,
}

pub struct IfStmt {
    op: Relop,
    lhs: Expression,
    rhs: Expression,
    then: LineNo,
}

pub struct ForStmt {
    var: Variable,
    from: Expression,
    to: Expression,
    step: Option<Expression>,
}

pub struct NextStmt {
    var: Variable,
}

pub struct DefStmt {
    func_id: u8, // 0 - 25 or A-Z
    var: Variable,
    expr: Expression,
}

// TODO
pub struct DimStmt {}
