mod visitor;

pub use self::visitor::Visitor;

use crate::ast::{Func, LineNo, Variable};
use crate::vm::value::FuncId;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Label(usize);

pub struct LabelIdGen {
    id: usize,
}

impl LabelIdGen {
    pub fn new() -> Self {
        LabelIdGen { id: 0 }
    }
    pub fn next_id(&mut self) -> Label {
        let label = Label(self.id);
        self.id += 1;
        label
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub label: Option<Label>,
    pub line_no: LineNo,
}

#[derive(Debug)]
pub enum InstructionKind {
    Data(f64),
    Constant(f64),
    Return,
    Jump(Label),
    JumpTrue(Label),
    JumpFalse(Label),
    Subroutine(Label),
    CallNative(Func),
    Call(Func),
    Stop,

    MapFunc(Func, FuncId),
    DefineGlobal(Variable),
    GetGlobal(Variable),
    SetGlobal(Variable),
    GetGlobalArray(Variable),
    SetGlobalArray(Variable),
    GetGlobalArray2d(Variable),
    SetGlobalArray2d(Variable),

    ReadGlobal(Variable),
    ReadGlobalArray(Variable),
    ReadGlobalArray2d(Variable),

    DefineLocal(Variable),
    SetLocal(Variable),
    GetLocal(Variable),

    InitArray(Variable),
    InitArray2d(Variable),
    SetArrayBound(Variable),
    SetArrayBound2d(Variable),

    PrintStart,
    PrintExpr,
    PrintLabel(String),
    PrintAdvance3,
    PrintAdvance15,
    PrintEnd,

    Dup,
    Negate,
    Not,
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    Equal,
    Less,
    Greater,

    LoopTest,

    Noop,
}
