use super::function::Func;
use super::keyword::Keyword;
use super::variable::Variable;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f64),       // 1.0, 2, 10, 3E10
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
    Comma,             // ,
    SemiColon,         // ;
    Eol,               // End of line
    Eof,               // End of input
}

impl Token {
    pub fn take(&mut self) -> Self {
        match self {
            Token::Label(s) => ::std::mem::replace(self, Token::Label(String::new())),
            _ => self.clone(),
        }
    }
    pub fn ty(&self) -> &'static str {
        use self::Token::*;

        match self {
            Number(_) => "Number",
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
