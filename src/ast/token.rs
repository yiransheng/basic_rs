use std::fmt;
use std::num;
use std::str;

use super::function::Func;
use super::keyword::Keyword;
use super::variable::{NameError, Variable};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Natural(u64),      // Natural Number 01, 12, 3339
    Real(f64),         // Float, 1.0, 1E10 1.2e-33
    Varname(Variable), // [A-Z]\d?
    Function(Func),    // see: function.rs
    Keyword(Keyword),  // see: keyword.rs
    Label(String),     // double-quoted string (no escaping)
    Equal,             // =
    Less,              // <
    Greater,           // >
    LessEqual,         // <=
    GreaterEqual,      // >=
    NotEqual,          // <>
    OpenParen,         // (
    CloseParen,        // )
    Plus,              // -
    Minus,             // +
    Star,              // *
    Slash,             // /
    CaretUp,           // ^
    Percent,           // %
    Comma,             // ,
    SemiColon,         // ;
    Eol,               // End of line
    Eof,               // End of input
}

impl Token {
    pub fn take(&mut self) -> Self {
        match self {
            Token::Label(_) => {
                ::std::mem::replace(self, Token::Label(String::new()))
            }
            _ => self.clone(),
        }
    }
    pub fn ty(&self) -> &'static str {
        use self::Token::*;

        match self {
            Natural(_) => "Natural",
            Real(_) => "Real",
            Varname(_) => "Varname",
            Function(_) => "Function",
            Keyword(_) => "Keyword",
            Label(_) => "Label",
            Equal => "Equal",
            Less => "Less",
            Greater => "Greater",
            LessEqual => "LessEqual",
            GreaterEqual => "GreaterEqual",
            NotEqual => "NotEqual",
            OpenParen => "OpenParen",
            CloseParen => "CloseParen",
            Plus => "Plus",
            Minus => "Minus",
            Star => "Star",
            Slash => "Slash",
            CaretUp => "CaretUp",
            Percent => "Percent",
            Comma => "Comma",
            SemiColon => "SemiColon",
            Eol => "Eol",
            Eof => "Eof",
        }
    }
}

impl From<Keyword> for Token {
    fn from(kw: Keyword) -> Token {
        Token::Keyword(kw)
    }
}
impl From<Func> for Token {
    fn from(func: Func) -> Token {
        Token::Function(func)
    }
}

pub enum FromStrError {
    Int(num::ParseIntError),
    Float(num::ParseFloatError),
    VarnameError(NameError),
    StringError(::std::string::ParseError),
    Unknown,
}

impl str::FromStr for Keyword {
    type Err = FromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "LET" => Ok(Keyword::Let),
            "READ" => Ok(Keyword::Read),
            "DATA" => Ok(Keyword::Data),
            "PRINT" => Ok(Keyword::Print),
            "GOTO" => Ok(Keyword::Goto),
            "IF" => Ok(Keyword::If),
            "FOR" => Ok(Keyword::For),
            "NEXT" => Ok(Keyword::Next),
            "END" => Ok(Keyword::End),
            "DEF" => Ok(Keyword::Def),
            "GOSUB" => Ok(Keyword::Gosub),
            "RETURN" => Ok(Keyword::Return),
            "DIM" => Ok(Keyword::Dim),
            "REM" => Ok(Keyword::Rem),
            "TO" => Ok(Keyword::To),
            "THEN" => Ok(Keyword::Then),
            "STEP" => Ok(Keyword::Step),
            "STOP" => Ok(Keyword::Stop),
            _ => Err(FromStrError::Unknown),
        }
    }
}

impl str::FromStr for Func {
    type Err = FromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SIN" => Ok(Func::Sin),
            "COS" => Ok(Func::Cos),
            "TAN" => Ok(Func::Tan),
            "ATN" => Ok(Func::Atn),
            "EXP" => Ok(Func::Exp),
            "ABS" => Ok(Func::Abs),
            "LOG" => Ok(Func::Log),
            "SQR" => Ok(Func::Sqr),
            "RND" => Ok(Func::Rnd),
            "INT" => Ok(Func::Int),
            "FNA" => Ok(Func::Fna),
            "FNB" => Ok(Func::Fnb),
            "FNC" => Ok(Func::Fnc),
            "FND" => Ok(Func::Fnd),
            "FNE" => Ok(Func::Fne),
            "FNF" => Ok(Func::Fnf),
            "FNG" => Ok(Func::Fng),
            "FNH" => Ok(Func::Fnh),
            "FNI" => Ok(Func::Fni),
            "FNJ" => Ok(Func::Fnj),
            "FNK" => Ok(Func::Fnk),
            "FNL" => Ok(Func::Fnl),
            "FNM" => Ok(Func::Fnm),
            "FNN" => Ok(Func::Fnn),
            "FNO" => Ok(Func::Fno),
            "FNP" => Ok(Func::Fnp),
            "FNQ" => Ok(Func::Fnq),
            "FNR" => Ok(Func::Fnr),
            "FNS" => Ok(Func::Fns),
            "FNT" => Ok(Func::Fnt),
            "FNU" => Ok(Func::Fnu),
            "FNV" => Ok(Func::Fnv),
            "FNW" => Ok(Func::Fnw),
            "FNX" => Ok(Func::Fnx),
            "FNY" => Ok(Func::Fny),
            "FNZ" => Ok(Func::Fnz),
            _ => Err(FromStrError::Unknown),
        }
    }
}

fn from_tagged_token_str<T: str::FromStr>(
    tag: &str,
    src: &str,
) -> Result<T, Option<T::Err>> {
    let value = src
        .trim_start_matches(tag)
        .trim_start_matches('(')
        .trim_end_matches(')');

    if value.len() + tag.len() + 2 == src.len() {
        T::from_str(value).map_err(Option::Some)
    } else {
        Err(None)
    }
}

impl str::FromStr for Token {
    type Err = FromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match from_tagged_token_str::<u64>("Natural", s) {
            Ok(n) => return Ok(Token::Natural(n)),
            Err(Some(e)) => return Err(FromStrError::Int(e)),
            _ => {}
        }
        match from_tagged_token_str::<f64>("Real", s) {
            Ok(n) => return Ok(Token::Real(n)),
            Err(Some(e)) => return Err(FromStrError::Float(e)),
            _ => {}
        }
        match from_tagged_token_str::<Variable>("Varname", s) {
            Ok(n) => return Ok(Token::Varname(n)),
            Err(Some(e)) => return Err(FromStrError::VarnameError(e)),
            _ => {}
        }
        match from_tagged_token_str::<Func>("Function", s) {
            Ok(n) => return Ok(Token::Function(n)),
            Err(Some(e)) => return Err(e),
            _ => {}
        }
        match from_tagged_token_str::<Keyword>("Keyword", s) {
            Ok(n) => return Ok(Token::Keyword(n)),
            Err(Some(e)) => return Err(e),
            _ => {}
        }
        match from_tagged_token_str::<String>("Label", s) {
            Ok(n) => return Ok(Token::Label(n)),
            Err(Some(e)) => return Err(FromStrError::StringError(e)),
            _ => {}
        }

        match s {
            "Equal" => Ok(Token::Equal),               // =
            "Less" => Ok(Token::Less),                 // <
            "Greater" => Ok(Token::Greater),           // >
            "LessEqual" => Ok(Token::LessEqual),       // <=
            "GreaterEqual" => Ok(Token::GreaterEqual), // >=
            "NotEqual" => Ok(Token::NotEqual),         // <>
            "OpenParen" => Ok(Token::OpenParen),       // (
            "CloseParen" => Ok(Token::CloseParen),     // )
            "Plus" => Ok(Token::Plus),                 // -
            "Minus" => Ok(Token::Minus),               // +
            "Star" => Ok(Token::Star),                 // *
            "Slash" => Ok(Token::Slash),               // /
            "CaretUp" => Ok(Token::CaretUp),           // ^
            "Percent" => Ok(Token::Percent),           // %
            "Comma" => Ok(Token::Comma),               // ,
            "SemiColon" => Ok(Token::SemiColon),       // ;
            "Eol" => Ok(Token::Eol),                   // End of line
            "Eof" => Ok(Token::Eof),                   // End of input
            _ => Err(FromStrError::Unknown),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.ty())?;
        match self {
            Token::Natural(x) => write!(f, "({})", x),
            Token::Real(x) => write!(f, "({})", x),
            Token::Varname(x) => write!(f, "({})", x),
            Token::Function(x) => write!(f, "({})", x),
            Token::Keyword(x) => write!(f, "({})", x),
            Token::Label(x) => write!(f, "({})", x),
            _ => Ok(()),
        }
    }
}
