use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, Eq, PartialEq, FromPrimitive, ToPrimitive)]
pub enum OpCode {
    Constant = 0x00,
    Return,
    ReturnValue,
    Jump,
    JumpTrue,
    JumpFalse,
    CallNative,
    CallIndirect,
    Call,
    Stop,

    Read,
    Rand,

    GetFunc,
    BindFunc,
    DeclLocal,
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

    PrintExpr,
    PrintLabel,
    PrintAdvance3,
    PrintAdvance15,
    PrintNewline,

    Negate,
    Not,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    CopySign,

    Equal,
    Less,
    Greater,

    Noop,
}

impl OpCode {
    pub fn short(self) -> &'static str {
        use self::OpCode::*;

        match self {
            Constant => "const",
            Return => "ret",
            Jump => "jmp",
            JumpTrue => "jmp.t",
            JumpFalse => "jmp.f",
            Subroutine => "sub",
            CallNative => "call.na",
            CallIndirect => "call_",
            Call => "call",
            Stop => "stop",

            Read => "read",
            Rand => "rand",

            Pop => "pop",
            SetFunc => "set.fn",
            BindFunc => "bind.fn",
            GetGlobal => "get.var",
            SetGlobal => "set.var",
            GetGlobalArray => "get.arr",
            SetGlobalArray => "set.arr",
            GetGlobalArray2d => "get.mat",
            SetGlobalArray2d => "set.mat",

            DeclLocal => "decl.loc",
            GetLocal => "get.loc",
            SetLocal => "set.loc",

            InitArray => "init.arr",
            InitArray2d => "init.mat",

            PrintExpr => "prt.expr",
            PrintLabel => "prt.lab",
            PrintAdvance3 => "prt;",
            PrintAdvance15 => "prt,",
            PrintNewline => "prt\\n",

            Dup => "dup",
            Negate => "neg",
            Not => "not",
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "div",
            Pow => "pow",
            CopySign => "copysign",

            Equal => "eq",
            Less => "lt",
            Greater => "gt",

            Noop => "noop",
        }
    }
}
