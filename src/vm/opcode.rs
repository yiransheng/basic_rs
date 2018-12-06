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
    SetFunc,
    GetFunc,
    GetGlobal,
    SetGlobal,
    GetGlobalArray,
    SetGlobalArray,
    GetGlobalArray2d,
    SetGlobalArray2d,

    GetLocal,

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
    Swap,
    Sign,
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

    Noop,
}
