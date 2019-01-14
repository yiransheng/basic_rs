mod builder;
pub mod optimize;

pub use self::builder::Builder;

use std::mem;

use crate::ast;
use slotmap::{new_key_type, SecondaryMap};
use std::collections::VecDeque;

new_key_type! { pub struct Label; }
new_key_type! { pub struct FunctionName; }

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum GlobalKind {
    Variable(ast::Variable),
    ArrPtr(ast::Variable, Offset<()>),
    FnPtr(ast::Func),
}
impl Into<ValueType> for GlobalKind {
    fn into(self) -> ValueType {
        match self {
            GlobalKind::Variable(_) => ValueType::F64,
            GlobalKind::ArrPtr(..) => ValueType::ArrPtr,
            GlobalKind::FnPtr(_) => ValueType::FnPtr,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ValueType {
    F64,
    ArrPtr,
    FnPtr,
}
#[derive(Debug, Copy, Clone)]
pub struct FnType {
    pub arg: Option<ValueType>,
    pub ret: Option<ValueType>,
}
impl FnType {
    pub fn def_type() -> Self {
        FnType {
            arg: Some(ValueType::F64),
            ret: Some(ValueType::F64),
        }
    }
}
impl Default for FnType {
    fn default() -> Self {
        FnType {
            arg: None,
            ret: None,
        }
    }
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
    pub name: FunctionName,
    pub line_no: ast::LineNo,
    pub ty: FnType,
    pub locals: Vec<ValueType>,
    pub entry: Label,
    pub blocks: SecondaryMap<Label, BasicBlock>,
}

impl Function {
    pub fn iter(&self) -> BlockIter {
        let mut stack = VecDeque::new();
        stack.push_back(self.entry);

        BlockIter {
            function: self,
            stack,
            visited: SecondaryMap::new(),
        }
    }
}

pub struct BlockIter<'a> {
    function: &'a Function,
    stack: VecDeque<Label>,
    visited: SecondaryMap<Label, ()>,
}

impl<'a> Iterator for BlockIter<'a> {
    type Item = &'a BasicBlock;

    fn next(&mut self) -> Option<Self::Item> {
        let mut label;
        loop {
            label = self.stack.pop_back()?;
            if self.visited.get(label).is_some() {
                continue;
            }
            self.visited.insert(label, ());
            break;
        }

        let block = match self.function.blocks.get(label) {
            Some(block) => block,
            None => {
                self.stack.clear();
                return None;
            }
        };
        match &block.exit {
            BlockExit::Return(_) => {}
            BlockExit::Jump(label) => {
                if self.visited.get(*label).is_none() {
                    self.stack.push_back(*label);
                }
            }
            BlockExit::Switch(_, true_br, None) => {
                if self.visited.get(*true_br).is_none() {
                    self.stack.push_back(*true_br);
                }
            }
            BlockExit::Switch(_, true_br, Some(false_br)) => {
                if self.visited.get(*true_br).is_none() {
                    self.stack.push_back(*true_br);
                }
                if self.visited.get(*false_br).is_none() {
                    self.stack.push_back(*false_br);
                }
            }
        }

        Some(block)
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub label: Label,
    pub statements: Vec<Statement>,
    pub line_nos: Vec<ast::LineNo>,
    pub exit: BlockExit,
}

impl BasicBlock {
    pub fn empty(label: Label) -> Self {
        BasicBlock {
            label,
            statements: vec![],
            line_nos: vec![],
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

#[derive(Debug, Clone)]
pub enum LValue {
    ArrPtr(ast::Variable, Offset),
    FnPtr(ast::Func),
    Global(ast::Variable),
    Local(usize),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum Offset<T = Expr> {
    OneD(T),
    TwoD(T, T),
}

#[derive(Debug, Clone)]
pub enum Expr {
    RandF64,
    ReadData,
    Input,
    Const(f64),
    Get(Box<LValue>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Call(Box<LValue>, Box<Expr>),
}

impl Expr {
    // basic local const folding
    pub fn evaluate_const(&mut self) {
        use std::ops::{Deref, DerefMut};

        match self {
            Expr::Call(_, expr) => {
                expr.evaluate_const();
            }
            Expr::Unary(op, expr) => {
                expr.evaluate_const();
                let expr = match (*expr).deref() {
                    Expr::Const(v) => f64::evaluate_unary(*op, *v),
                    _ => None,
                };
                if let Some(n) = expr {
                    *self = Expr::Const(n);
                }
            }
            Expr::Binary(op, lhs, rhs) => {
                lhs.evaluate_const();
                rhs.evaluate_const();

                let lhs = (*lhs).deref_mut();
                let rhs = (*rhs).deref_mut();

                match (op, lhs, rhs) {
                    (BinaryOp::Add, Expr::Const(z), ref mut rhs)
                        if 0.0.eq(z) =>
                    {
                        let rhs = mem::replace(*rhs, Expr::ReadData);
                        *self = rhs;
                        return;
                    }
                    (BinaryOp::Add, ref mut lhs, Expr::Const(z))
                        if 0.0.eq(z) =>
                    {
                        let lhs = mem::replace(*lhs, Expr::ReadData);
                        *self = lhs;
                        return;
                    }
                    (BinaryOp::Mul, Expr::Const(z), _) if 0.0.eq(z) => {
                        *self = Expr::Const(0.0);
                        return;
                    }
                    (BinaryOp::Mul, _, Expr::Const(z)) if 0.0.eq(z) => {
                        *self = Expr::Const(0.0);
                        return;
                    }
                    (BinaryOp::Mul, Expr::Const(v), ref mut rhs)
                        if 1.0.eq(v) =>
                    {
                        let rhs = mem::replace(*rhs, Expr::ReadData);
                        *self = rhs;
                        return;
                    }
                    (BinaryOp::Mul, ref mut lhs, Expr::Const(v))
                        if 1.0.eq(v) =>
                    {
                        let lhs = mem::replace(*lhs, Expr::ReadData);
                        *self = lhs;
                        return;
                    }
                    (BinaryOp::Mul, Expr::Const(v), ref mut rhs)
                        if (-1.0).eq(v) =>
                    {
                        let rhs = mem::replace(*rhs, Expr::ReadData);
                        *self = Expr::Unary(UnaryOp::Neg, Box::new(rhs));
                        return;
                    }
                    (BinaryOp::Mul, ref mut lhs, Expr::Const(v))
                        if (-1.0).eq(v) =>
                    {
                        let lhs = mem::replace(*lhs, Expr::ReadData);
                        *self = Expr::Unary(UnaryOp::Neg, Box::new(lhs));
                        return;
                    }
                    (ref op, Expr::Const(lhs), Expr::Const(rhs)) => {
                        if let Some(n) = f64::evaluate_binary(**op, *lhs, *rhs)
                        {
                            *self = Expr::Const(n);
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
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
    Rem,
    Pow,
    Less,
    Greater,
    CopySign,
    Equal,
}

trait EvalBinary<T> {
    fn evaluate_binary(_op: BinaryOp, _lhs: Self, _rhs: Self) -> Option<T>
    where
        Self: Sized,
    {
        None
    }
}
trait EvalUnary<T> {
    fn evaluate_unary(_op: UnaryOp, _operand: Self) -> Option<T>
    where
        Self: Sized,
    {
        None
    }
}
impl EvalUnary<f64> for f64 {
    fn evaluate_unary(op: UnaryOp, operand: Self) -> Option<f64> {
        match op {
            UnaryOp::Neg => Some(-operand),
            UnaryOp::Sin => Some(operand.sin()),
            UnaryOp::Cos => Some(operand.cos()),
            UnaryOp::Atn => Some(operand.atan()),
            UnaryOp::Exp => Some(operand.exp()),
            UnaryOp::Abs => Some(operand.abs()),
            UnaryOp::Log => Some(operand.ln()),
            UnaryOp::Sqr => Some(operand.sqrt()),
            UnaryOp::Trunc => Some(operand.trunc()),
            _ => None,
        }
    }
}

impl EvalBinary<f64> for f64 {
    fn evaluate_binary(op: BinaryOp, lhs: Self, rhs: Self) -> Option<Self> {
        match op {
            BinaryOp::Add => Some(lhs + rhs),
            BinaryOp::Sub => Some(lhs - rhs),
            BinaryOp::Mul => Some(lhs * rhs),
            BinaryOp::Div => Some(lhs / rhs),
            BinaryOp::Pow => Some(lhs.powf(rhs)),
            // copysign is unstable now
            BinaryOp::CopySign => Some(lhs * rhs.signum()),
            _ => None,
        }
    }
}

mod print {
    use std::fmt::{self, Display};

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
                for block in func.iter() {
                    let label = block.label;
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

    impl Display for ValueType {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                ValueType::F64 => "f64".fmt(f),
                ValueType::ArrPtr => "*array".fmt(f),
                ValueType::FnPtr => "*fn".fmt(f),
            }
        }
    }
    impl Display for FnType {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match (self.arg, self.ret) {
                (Some(x), Some(y)) => write!(f, "fn({}) -> {}", x, y),
                (Some(x), None) => write!(f, "fn({})", x),
                (None, Some(y)) => write!(f, "fn() -> {}", y),
                (None, None) => write!(f, "fn()"),
            }
        }
    }

    impl Display for GlobalKind {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                GlobalKind::Variable(var) => write!(f, "Var({})", var),
                GlobalKind::ArrPtr(var, Offset::OneD(..)) => {
                    write!(f, "Array1d({})", var)
                }
                GlobalKind::ArrPtr(var, Offset::TwoD(..)) => {
                    write!(f, "Array2d({})", var)
                }
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
                Expr::Input => write!(f, "input()"),
                Expr::ReadData => write!(f, "read()"),
                Expr::RandF64 => write!(f, "random()"),
                Expr::Const(n) => write!(f, "{}", n),
                Expr::Get(lval) => lval.fmt(f),
                Expr::Binary(op, lhs, rhs) => match op {
                    BinaryOp::Add => write!(f, "({} + {})", lhs, rhs),
                    BinaryOp::Sub => write!(f, "({} - {})", lhs, rhs),
                    BinaryOp::Mul => write!(f, "({} * {})", lhs, rhs),
                    BinaryOp::Div => write!(f, "({} / {})", lhs, rhs),
                    BinaryOp::Rem => write!(f, "({} % {})", lhs, rhs),
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
                Statement::Print(v) => env.fmtln(format_args!("print({})", v)),
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
                Statement::PrintNewline => env.fmtln("println()"),
            }
        }
    }
    impl Printable for Function {
        fn print(&self, env: &mut PrintEnv) -> Result<(), fmt::Error> {
            let name = self.name.named(&env.names);
            let line_no = self.line_no;
            env.fmtln(format_args!("function {}", name,))?;

            env.indented(|env| {
                env.fmtln(format_args!("defined on Line: {}", line_no))?;
                env.fmtln(format_args!("type: {}", self.ty))?;
                for (i, local) in self.locals.iter().enumerate() {
                    env.fmtln(format_args!("local({}): {}", i, local))?;
                }
                for block in self.iter() {
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
            if !self.line_nos.is_empty() {
                let first = self.line_nos.first().unwrap();
                let last = self.line_nos.last().unwrap();
                env.fmtln(format_args!(
                    "{} Line {} - {}: ",
                    label, first, last
                ))?;
            } else {
                env.fmtln(label)?;
            }

            env.indented(|env| {
                for stmt in &self.statements {
                    stmt.print(env)?;
                }
                match &self.exit {
                    BlockExit::Return(None) => env.fmtln("return"),
                    BlockExit::Return(Some(v)) => {
                        env.fmtln(format_args!("return {}", v))
                    }
                    BlockExit::Jump(label) => env.fmtln(format_args!(
                        "goto {}",
                        label.named(&env.names)
                    )),
                    BlockExit::Switch(cond, true_br, false_br) => {
                        env.fmtln(format_args!("if {}:", cond))?;
                        env.indented(|env| {
                            env.fmtln(format_args!(
                                "goto {}",
                                true_br.named(&env.names)
                            ))
                        })?;
                        if let Some(ref label) = false_br {
                            env.fmtln("else:")?;
                            env.indented(|env| {
                                env.fmtln(format_args!(
                                    "goto {}",
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

            env.fmt("data:")?;
            for datum in self.data.iter().rev() {
                env.fmt(format_args!(" {}", datum))?;
            }
            env.fmtln("")?;
            env.fmtln("")?;

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
