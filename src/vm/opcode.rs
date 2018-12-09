use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, Eq, PartialEq, FromPrimitive, ToPrimitive)]
pub enum OpCode {
    Constant = 0x00,
    FnConstant,
    Return,
    Jump,
    JumpTrue,
    JumpFalse,
    Subroutine,
    CallNative,
    Call,
    Stop,

    Pop,
    ReadGlobal,
    ReadGlobalArray,
    ReadGlobalArray2d,

    SetFunc,
    GetFunc,
    GetGlobal,
    SetGlobal,
    GetGlobalArray,
    SetGlobalArray,
    GetGlobalArray2d,
    SetGlobalArray2d,

    GetLocal,
    SetLocal,

    InitArray,
    InitArray2d,
    SetArrayBound,
    SetArrayBound2d,

    PrintStart,
    PrintExpr,
    PrintLabel,
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

impl OpCode {
    pub fn short(self) -> &'static str {
        use self::OpCode::*;

        match self {
            Constant => "const",
            FnConstant => "fn",
            Return => "ret",
            Jump => "jmp",
            JumpTrue => "jmp.t",
            JumpFalse => "jmp.f",
            Subroutine => "sub",
            CallNative => "call.na",
            Call => "call",
            Stop => "stop",

            ReadGlobal => "read.var",
            ReadGlobalArray => "read.arr",
            ReadGlobalArray2d => "read.mat",

            Pop => "pop",
            SetFunc => "set.fn",
            GetFunc => "get.fn",
            GetGlobal => "get.var",
            SetGlobal => "set.var",
            GetGlobalArray => "get.arr",
            SetGlobalArray => "set.arr",
            GetGlobalArray2d => "get.mat",
            SetGlobalArray2d => "set.mat",

            GetLocal => "get.loc",
            SetLocal => "set.loc",

            InitArray => "init.arr",
            InitArray2d => "init.mat",
            SetArrayBound => "set.len",
            SetArrayBound2d => "set.dim",

            PrintStart => "prt",
            PrintExpr => "prt.expr",
            PrintLabel => "prt.lab",
            PrintAdvance3 => "prt;",
            PrintAdvance15 => "prt,",
            PrintEnd => "prt.end",

            Dup => "dup",
            Negate => "neg",
            Not => "not",
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "div",
            Pow => "pow",

            Equal => "eq",
            Less => "lt",
            Greater => "gt",

            LoopTest => "loop.test",

            Noop => "noop",
        }
    }
}
