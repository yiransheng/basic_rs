mod builder;
mod codegen;

pub use self::builder::Builder;

use std::fmt;

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

impl fmt::Display for GlobalKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            GlobalKind::Variable(var) => write!(f, "Var({})", var),
            GlobalKind::ArrPtr(var) => write!(f, "Array({})", var),
            GlobalKind::FnPtr(var) => write!(f, "Function({})", var),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub globals: Vec<GlobalKind>,
    pub functions: Vec<Function>,
    pub main: FunctionName,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "globals:")?;
        for kind in &self.globals {
            write!(f, " {}", kind)?;
        }
        writeln!(f)?;
        writeln!(f)?;

        for (i, func) in self.functions.iter().enumerate() {
            if func.name == self.main {
                func.print(f, "main")?;
            } else {
                func.print(f, i)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Function {
    name: FunctionName,
    local_count: usize,
    entry: Label,
    blocks: SecondaryMap<Label, BasicBlock>,
}

impl Function {
    fn print<N: fmt::Display>(
        &self,
        f: &mut fmt::Formatter,
        name: N,
    ) -> Result<(), fmt::Error> {
        let mut labels: SecondaryMap<Label, usize> = SecondaryMap::new();
        let mut get_label_index = move |label| {
            if let Some(i) = labels.get(label) {
                *i
            } else {
                let n = labels.len();
                labels.insert(label, n);
                n
            }
        };

        writeln!(f, "fn${} locals({}):", name, self.local_count)?;

        for (label, block) in self.blocks.iter() {
            writeln!(f, "L{}:", get_label_index(label));
            for stmt in &block.statements {
                writeln!(f, "  {}", stmt)?;
            }
            match &block.exit {
                BlockExit::Return(None) => {
                    writeln!(f, "  return")?;
                }
                BlockExit::Return(Some(v)) => {
                    writeln!(f, "  return {}", v)?;
                }
                BlockExit::Jump(label) => {
                    writeln!(f, "  jmp L{}", get_label_index(*label))?;
                }
                BlockExit::Switch(cond, true_br, false_br) => {
                    writeln!(f, "  if {}:", cond)?;
                    writeln!(f, "    jmp L{}", get_label_index(*true_br))?;
                    match false_br {
                        Some(label) => {
                            writeln!(f, "  else:")?;
                            writeln!(
                                f,
                                "    jmp L{}",
                                get_label_index(*label)
                            )?;
                        }
                        _ => {}
                    }
                }
            }
        }

        writeln!(f)?;

        Ok(())
    }
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

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            LValue::ArrPtr(var, offset) => match offset {
                Offset::OneD(expr) => write!(f, "${}[{}]", var, expr),
                Offset::TwoD(i, j) => write!(f, "${}[{}, {}]", var, i, j),
            },
            LValue::FnPtr(func) => write!(f, "{}", func),
            LValue::Global(var) => write!(f, "${}", var),
            LValue::Local(index) => write!(f, "${}", index),
        }
    }
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
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Expr::RandF64 => write!(f, "<random>"),
            Expr::Const(n) => write!(f, "{}", n),
            Expr::Get(lval) => lval.fmt(f),
            Expr::Binary(op, lhs, rhs) => match op {
                BinaryOp::Add => write!(f, "{} + {}", lhs, rhs),
                BinaryOp::Sub => write!(f, "{} - {}", lhs, rhs),
                BinaryOp::Mul => write!(f, "{} * {}", lhs, rhs),
                BinaryOp::Div => write!(f, "{} / {}", lhs, rhs),
                BinaryOp::Pow => write!(f, "{} ^ {}", lhs, rhs),
                BinaryOp::Less => write!(f, "{} < {}", lhs, rhs),
                BinaryOp::Greater => write!(f, "{} > {}", lhs, rhs),
                BinaryOp::Equal => write!(f, "{} == {}", lhs, rhs),
                BinaryOp::CopySign => write!(f, "copysign({}, {})", lhs, rhs),
            },
            Expr::Unary(op, rhs) => match op {
                UnaryOp::Not => write!(f, "!{}", rhs),
                UnaryOp::Neg => write!(f, "-{}", rhs),
                UnaryOp::Sin => write!(f, "sin({})", rhs),
                UnaryOp::Cos => write!(f, "cos({})", rhs),
                UnaryOp::Atn => write!(f, "atn({})", rhs),
                UnaryOp::Exp => write!(f, "exp({})", rhs),
                UnaryOp::Abs => write!(f, "abs({})", rhs),
                UnaryOp::Log => write!(f, "log({})", rhs),
                UnaryOp::Sqr => write!(f, "sqrt({})", rhs),
                UnaryOp::Trunc => write!(f, "trunc({})", rhs),
            },
            Expr::Call(func, expr) => write!(f, "{}({})", func, expr),
        }
    }
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
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut funcs: SecondaryMap<FunctionName, usize> = SecondaryMap::new();
        let mut get_func_index = move |name| {
            if let Some(i) = funcs.get(name) {
                *i
            } else {
                let n = funcs.len();
                funcs.insert(name, n);
                n
            }
        };

        match self {
            Statement::Assign(v, expr) => write!(f, "{} <- {};", v, expr),
            Statement::DefFn(v, name) => {
                write!(f, "{} <- fn_{};", v, get_func_index(*name))
            }
            Statement::Alloc1d(v, expr) => {
                write!(f, "alloc_1d {} {};", v, expr)
            }
            Statement::Alloc2d(v, m, n) => {
                write!(f, "alloc_2d {} {} x {}", v, m, n)
            }
            Statement::CallSub(name) => {
                write!(f, "fn_{}();", get_func_index(*name))
            }
            Statement::Print(v) => write!(f, "print({});", v),
            Statement::PrintLabel(s) => write!(f, "print({});", s),
            _ => Ok(()),
        }
    }
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
}
