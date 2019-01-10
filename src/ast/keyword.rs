use crate::scanner::KeywordToken;
use keyword_token_derive::*;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(
    KeywordToken,
    Debug,
    Copy,
    Clone,
    Eq,
    PartialEq,
    Hash,
    FromPrimitive,
    ToPrimitive,
)]
pub enum Keyword {
    Let,
    Read,
    Data,
    Print,
    Goto,
    If,
    Input,
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
