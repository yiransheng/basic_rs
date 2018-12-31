mod builder;
mod codegen;

pub use self::builder::Builder;
pub use self::codegen::CodeGen;

use std::fmt;

use basic_rs::ast;
use slotmap::{new_key_type, SecondaryMap};

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
    pub globals: Vec<GlobalKind>,
    pub functions: Vec<Function>,
    pub main: FunctionName,
    pub data: Vec<f64>,
    pub labels: String,
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
    ReadData,
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
    // offset, len in bytes
    PrintLabel(usize, usize),
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
}

mod print {
    use std::fmt::{self, Display};
    use std::marker::PhantomData;

    use super::*;

    trait Printable {
        fn print(&self, env: &mut PrintEnv) -> Result<(), fmt::Error>;
    }

    trait Named {
        type Name: Display;

        fn named(&self, names: &Names) -> Self::Name
        where
            Self: Sized;
    }

    impl Named for Label {
        type Name = IndexedName;

        fn named(&self, names: &Names) -> IndexedName {
            let id_index = names.labels.get(*self).cloned();
            IndexedName {
                id_index,
                prefix: "L",
            }
        }
    }
    impl Named for FunctionName {
        type Name = IndexedName;

        fn named(&self, names: &Names) -> IndexedName {
            if *self == names.main {
                IndexedName {
                    id_index: None,
                    prefix: "main",
                }
            } else {
                let id_index = names.funcs.get(*self).cloned();
                IndexedName {
                    id_index,
                    prefix: "fn_",
                }
            }
        }
    }

    // helper structs
    struct IndexedName {
        prefix: &'static str,
        id_index: Option<usize>,
    }
    impl Display for IndexedName {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            write!(f, "{}", self.prefix)?;
            if let Some(id_index) = self.id_index {
                write!(f, "{}", id_index)?;
            }

            Ok(())
        }
    }

    struct Names<'a> {
        strings: &'a str,
        main: FunctionName,
        funcs: SecondaryMap<FunctionName, usize>,
        labels: SecondaryMap<Label, usize>,
    }

    impl<'a> Names<'a> {
        fn from_ir(ir: &'a Program) -> Self {
            let mut counter_func: usize = 0;
            let mut counter_label: usize = 0;
            let mut funcs = SecondaryMap::new();
            let mut labels = SecondaryMap::new();

            for func in &ir.functions {
                funcs.insert(func.name, counter_func);
                counter_func += 1;
                for label in func.blocks.keys() {
                    labels.insert(label, counter_label);
                    counter_label += 1;
                }
            }

            Names {
                strings: &ir.labels,
                funcs,
                labels,
                main: ir.main,
            }
        }
    }

    struct PrintEnv<'p, 'a: 'p> {
        formatter: &'p mut fmt::Formatter<'a>,
        names: Names<'p>,
        indent_level: usize,
        indent: &'p str,
    }
    impl<'p, 'a: 'p> PrintEnv<'p, 'a> {
        fn fmt<T: Display>(&mut self, x: T) -> Result<(), fmt::Error> {
            for _ in 0..self.indent_level {
                write!(self.formatter, "{}", self.indent)?;
            }
            x.fmt(self.formatter)
        }
        fn fmtln<T: Display>(&mut self, x: T) -> Result<(), fmt::Error> {
            self.fmt(x)?;
            writeln!(self.formatter)
        }
        fn indented<F>(&mut self, f: F) -> Result<(), fmt::Error>
        where
            F: Fn(&mut Self) -> Result<(), fmt::Error>,
        {
            self.indent_level += 1;
            let r = f(self);
            self.indent_level -= 1;
            r
        }
    }

    impl Display for GlobalKind {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                GlobalKind::Variable(var) => write!(f, "Var({})", var),
                GlobalKind::ArrPtr(var) => write!(f, "Array({})", var),
                GlobalKind::FnPtr(var) => write!(f, "Function({})", var),
            }
        }
    }
    impl fmt::Display for LValue {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                LValue::ArrPtr(var, offset) => match offset {
                    Offset::OneD(expr) => write!(f, "${}[{}]", var, expr),
                    Offset::TwoD(i, j) => write!(f, "${}[{}, {}]", var, i, j),
                },
                LValue::FnPtr(func) => write!(f, "{}", func),
                LValue::Global(var) => write!(f, "{}", var),
                LValue::Local(index) => write!(f, "${}", index),
            }
        }
    }
    impl fmt::Display for Expr {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                Expr::ReadData => write!(f, "read()"),
                Expr::RandF64 => write!(f, "random()"),
                Expr::Const(n) => write!(f, "{}", n),
                Expr::Get(lval) => lval.fmt(f),
                Expr::Binary(op, lhs, rhs) => match op {
                    BinaryOp::Add => write!(f, "({} + {})", lhs, rhs),
                    BinaryOp::Sub => write!(f, "({} - {})", lhs, rhs),
                    BinaryOp::Mul => write!(f, "({} * {})", lhs, rhs),
                    BinaryOp::Div => write!(f, "({} / {})", lhs, rhs),
                    BinaryOp::Pow => write!(f, "{} ^ {}", lhs, rhs),
                    BinaryOp::Less => write!(f, "{} < {}", lhs, rhs),
                    BinaryOp::Greater => write!(f, "{} > {}", lhs, rhs),
                    BinaryOp::Equal => write!(f, "{} == {}", lhs, rhs),
                    BinaryOp::CopySign => {
                        write!(f, "copysign({}, {})", lhs, rhs)
                    }
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
    impl Printable for Statement {
        fn print(&self, env: &mut PrintEnv) -> Result<(), fmt::Error> {
            match self {
                Statement::Assign(v, expr) => {
                    env.fmtln(format_args!("{} <- {}", v, expr))
                }
                Statement::DefFn(v, name) => {
                    let name = name.named(&env.names);
                    env.fmtln(format_args!("{} <- {}", v, name))
                }
                Statement::Alloc1d(v, expr) => {
                    env.fmtln(format_args!("alloc_1d {} {};", v, expr))
                }
                Statement::Alloc2d(v, m, n) => {
                    env.fmtln(format_args!("alloc_2d {} {} x {}", v, m, n))
                }
                Statement::CallSub(name) => {
                    let name = name.named(&env.names);
                    env.fmtln(format_args!("{}();", name))
                }
                Statement::Print(v) => env.fmtln(format_args!("print({});", v)),
                Statement::PrintLabel(offset, length) => {
                    let offset = *offset;
                    let length = *length;
                    let s = ::std::str::from_utf8(
                        &env.names.strings.as_bytes()[offset..offset + length],
                    )
                    .unwrap();
                    env.fmtln(format_args!("print(\"{}\")", s))
                }
                Statement::PrintAdvance3 => env.fmtln("print(;)"),
                Statement::PrintAdvance15 => env.fmtln("print(,)"),
                Statement::PrintNewline => env.fmtln("print()"),
            }
        }
    }
    impl Printable for Function {
        fn print(&self, env: &mut PrintEnv) -> Result<(), fmt::Error> {
            let name = self.name.named(&env.names);
            env.fmtln(format_args!(
                "function {} locals: {}",
                name, self.local_count
            ))?;

            env.indented(|env| {
                for (_, block) in self.blocks.iter() {
                    if let Err(err) = block.print(env) {
                        return Err(err);
                    }
                }

                Ok(())
            })
        }
    }

    impl Printable for BasicBlock {
        fn print(&self, env: &mut PrintEnv) -> Result<(), fmt::Error> {
            let label = self.label.named(&env.names);
            env.fmtln(label)?;
            env.indented(|env| {
                for stmt in &self.statements {
                    match stmt.print(env) {
                        Ok(_) => {}
                        Err(err) => return Err(err),
                    }
                }
                match &self.exit {
                    BlockExit::Return(None) => env.fmtln("return"),
                    BlockExit::Return(Some(v)) => {
                        env.fmtln(format_args!("return {}", v))
                    }
                    BlockExit::Jump(label) => env
                        .fmtln(format_args!("jmp {}", label.named(&env.names))),
                    BlockExit::Switch(cond, true_br, false_br) => {
                        env.fmtln(format_args!("if {}:", cond))?;
                        env.indented(|env| {
                            env.fmtln(format_args!(
                                "jmp {}",
                                true_br.named(&env.names)
                            ))
                        })?;
                        if let Some(ref label) = false_br {
                            env.fmtln("else:")?;
                            env.indented(|env| {
                                env.fmtln(format_args!(
                                    "jmp {}",
                                    label.named(&env.names)
                                ))
                            })?;
                        }

                        Ok(())
                    }
                }
            })
        }
    }
    impl fmt::Display for Program {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            let mut env = PrintEnv {
                formatter: f,
                names: Names::from_ir(self),
                indent_level: 0,
                indent: "  ",
            };

            env.fmt("globals:")?;
            for kind in &self.globals {
                env.fmt(format_args!(" {}", kind))?;
            }
            env.fmtln("")?;
            env.fmtln("")?;

            for func in &self.functions {
                func.print(&mut env)?;
            }

            Ok(())
        }
    }

}
