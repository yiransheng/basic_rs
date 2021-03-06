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
    Input,
    Rand,

    GetFunc,
    BindFunc,
    DeclLocal,
    GetGlobal,
    SetGlobal,
    GetGlobalArray1d,
    SetGlobalArray1d,
    GetGlobalArray2d,
    SetGlobalArray2d,

    GetLocal,
    SetLocal,

    InitArray1d,
    InitArray2d,

    DefineDim1d,
    DefineDim2d,

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
    Rem,
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
            ReturnValue => "ret.val",
            Jump => "jmp",
            JumpTrue => "jmp.t",
            JumpFalse => "jmp.f",
            CallNative => "call.na",
            CallIndirect => "call_",
            Call => "call",
            Stop => "stop",

            Read => "read",
            Input => "input",
            Rand => "rand",

            GetFunc => "get.fn",
            BindFunc => "bind.fn",
            GetGlobal => "get.var",
            SetGlobal => "set.var",
            GetGlobalArray1d => "get.arr",
            SetGlobalArray1d => "set.arr",
            GetGlobalArray2d => "get.mat",
            SetGlobalArray2d => "set.mat",

            DeclLocal => "decl.loc",
            GetLocal => "get.loc",
            SetLocal => "set.loc",

            InitArray1d => "init.arr",
            InitArray2d => "init.mat",

            DefineDim1d => "dim.arr",
            DefineDim2d => "dim.mat",

            PrintExpr => "prt.expr",
            PrintLabel => "prt.lab",
            PrintAdvance3 => "prt;",
            PrintAdvance15 => "prt,",
            PrintNewline => "prt\\n",

            Negate => "neg",
            Not => "not",
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "div",
            Rem => "rem",
            Pow => "pow",
            CopySign => "copysign",

            Equal => "eq",
            Less => "lt",
            Greater => "gt",

            Noop => "noop",
        }
    }
}
