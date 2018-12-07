use num_derive::{FromPrimitive, ToPrimitive};

#[derive(
    Debug, Copy, Clone, Eq, PartialEq, Hash, FromPrimitive, ToPrimitive,
)]
pub enum Keyword {
    Let,
    Read,
    Data,
    Print,
    Goto,
    If,
    For,
    Next,
    End,
    Def,
    Gosub,
    Return,
    Dim,
    Rem,
    To,
    Then,
    Step,
    Stop,
}
