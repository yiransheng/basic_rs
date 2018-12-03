use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, Eq, PartialEq, FromPrimitive, ToPrimitive)]
pub enum OpCode {
    Constant = 0x00,
    Return,
    Jump,
    CondJump,
    Subroutine,
    CallNative,
    Call,
    Stop,

    Pop,
    GetGlobal,
    SetGlobal,
    GetGlobalArray,
    SetGlobalArray,
    GetGlobalArray2d,
    SetGlobalArray2d,

    Print,

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
