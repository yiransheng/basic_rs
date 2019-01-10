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
pub enum Func {
    Sin,
    Cos,
    Tan,
    Atn,
    Exp,
    Abs,
    Log,
    Sqr,
    Rnd,
    Int,
    Fna,
    Fnb,
    Fnc,
    Fnd,
    Fne,
    Fnf,
    Fng,
    Fnh,
    Fni,
    Fnj,
    Fnk,
    Fnl,
    Fnm,
    Fnn,
    Fno,
    Fnp,
    Fnq,
    Fnr,
    Fns,
    Fnt,
    Fnu,
    Fnv,
    Fnw,
    Fnx,
    Fny,
    Fnz,
}

impl Func {
    pub fn is_native(&self) -> bool {
        match self {
            Func::Sin => true,
            Func::Cos => true,
            Func::Tan => true,
            Func::Atn => true,
            Func::Exp => true,
            Func::Abs => true,
            Func::Log => true,
            Func::Sqr => true,
            Func::Rnd => true,
            Func::Int => true,
            _ => false,
        }
    }
}
