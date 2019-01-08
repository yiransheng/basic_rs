mod dfa;
mod function;
mod keyword;
mod number;

use std::error;
use std::fmt;
use std::iter::IntoIterator;

use crate::ast::keyword::Keyword;
use crate::ast::{NameError, Token, Variable};

use self::dfa::Dfa;
use self::function::FuncDFA;
use self::keyword::KeywordDFA;
use self::number::MatchNum;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SourceMapped<T> {
    pub value: T,
    pub loc: SourceLoc,
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SourceLoc {
    pub line: usize,
    pub col: usize,
}

impl<T> SourceMapped<T> {
    pub fn map<U, F>(self, f: F) -> SourceMapped<U>
    where
        F: Fn(T) -> U,
    {
        SourceMapped {
            value: f(self.value),
            loc: self.loc,
        }
    }
}

impl<T> fmt::Display for SourceMapped<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "Line:{} Col:{}", self.loc.line, self.loc.col)?;
        self.value.fmt(f)
    }
}

impl<E> error::Error for SourceMapped<E>
where
    E: error::Error,
{
    fn description(&self) -> &str {
        self.value.description()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Error {
    UnexpectedChar(char),
    UnexpectedLineBreak,
    UnexpectedEof,
    BadNumber,
    BadIdentifier(Option<NameError>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let desc = error::Error::description(self);
        match self {
            Error::UnexpectedChar(c) => write!(f, "{}: {}", desc, c),
            _ => f.write_str(desc),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self {
            Error::UnexpectedChar(_) => "Unexpected character",
            Error::UnexpectedLineBreak => "Unexpected line break",
            Error::UnexpectedEof => "Unexpected end of input",
            Error::BadNumber => "Malformed number",
            Error::BadIdentifier(Some(e)) => e.description(),
            Error::BadIdentifier(None) => "Invalid identifier",
        }
    }
}

impl From<NameError> for Error {
    fn from(err: NameError) -> Self {
        Error::BadIdentifier(Some(err))
    }
}

pub type ScanResult = Result<SourceMapped<Token>, SourceMapped<Error>>;

fn match_ident(s: &str) -> Result<(Token, usize), Error> {
    let mut d = KeywordDFA::default()
        .map(|kw| kw.into())
        .alternative::<FuncDFA>();

    d.match_str(s).ok_or(Error::BadIdentifier(None))
}

fn match_number(s: &str) -> Result<(Token, usize), Error> {
    MatchNum.match_str(s).ok_or(Error::BadNumber)
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
    line_start: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(s: &'a str) -> Self {
        Scanner {
            source: s,
            start: 0,
            current: 0,
            line: 0,
            line_start: 0,
        }
    }

    pub fn scan(&mut self) -> ScanResult {
        match self.scan_token() {
            Ok(t) => {
                let token = self.with_loc(t);
                match &token.value {
                    Token::Keyword(Keyword::Rem) => self.skip_line(),
                    Token::Eol => {
                        self.line += 1;
                        self.line_start = self.current;
                    }
                    _ => {}
                }
                Ok(token)
            }
            Err(e) => Err(self.with_loc(e)),
        }
    }
}

pub struct ScannerIter<'a> {
    inner: Option<Scanner<'a>>,
}

impl<'a> Iterator for ScannerIter<'a> {
    type Item = ScanResult;

    fn next(&mut self) -> Option<ScanResult> {
        let item = self.inner.as_mut().map(|s| s.scan());

        match item {
            Some(Ok(ref t)) => {
                if let Token::Eof = &t.value {
                    self.inner = None;
                }
            }
            Some(Err(_)) => {
                self.inner = None;
            }
            _ => {}
        }

        item
    }
}

impl<'a> IntoIterator for Scanner<'a> {
    type Item = ScanResult;
    type IntoIter = ScannerIter<'a>;

    fn into_iter(self) -> ScannerIter<'a> {
        ScannerIter { inner: Some(self) }
    }
}

impl<'a> Scanner<'a> {
    fn with_loc<T>(&self, value: T) -> SourceMapped<T> {
        let loc = SourceLoc {
            line: self.line,
            col: self.start - self.line_start,
        };

        SourceMapped { value, loc }
    }

    fn scan_token(&mut self) -> Result<Token, Error> {
        self.skip_whites();
        self.start = self.current;

        let c = self.advance();
        let c = match c {
            Some(c) => c,
            None => return Ok(Token::Eof),
        };

        let token = match c {
            // 1 char tokens
            '\n' => Token::Eol,
            '=' => Token::Equal,
            ',' => Token::Comma,
            ';' => Token::SemiColon,
            '-' => Token::Minus,
            '+' => Token::Plus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '^' => Token::CaretUp,
            '%' => Token::Percent,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            // 2 chars tokens
            '<' => match self.peek() {
                Some('=') => {
                    self.advance();
                    Token::LessEqual
                }
                Some('>') => {
                    self.advance();
                    Token::NotEqual
                }
                _ => Token::Less,
            },
            '>' => match self.peek() {
                Some('=') => {
                    self.advance();
                    Token::GreaterEqual
                }
                _ => Token::Greater,
            },
            // label
            '"' => self.scan_label()?,
            // . 0-9
            c if is_numeric(c) => {
                let rest = self.remainder().ok_or(Error::UnexpectedEof)?;
                let (token, consumed) = match_number(rest)?;
                self.current = self.start + consumed;
                token
            }
            // a-z; A-Z
            c if is_alpha(c) => match self.peek() {
                Some(d) if is_numeric(d) => {
                    self.advance();
                    let var = Variable::from_bytes(upper(c) as u8, d as u8)?;
                    Token::Varname(var)
                }
                Some(d) if is_alpha(d) => {
                    let rest = self.remainder().ok_or(Error::UnexpectedEof)?;
                    let (token, consumed) = match_ident(rest)?;
                    self.current = self.start + consumed;
                    token
                }
                _ => {
                    let var = Variable::from_byte(upper(c) as u8)?;
                    Token::Varname(var)
                }
            },
            c => return Err(Error::UnexpectedChar(c)),
        };

        Ok(token)
    }

    fn skip_line(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c != '\n' => {
                    self.advance();
                }
                _ => return,
            }
        }
    }

    fn skip_whites(&mut self) {
        loop {
            match self.peek() {
                Some(c) if is_white_excluding_newline(c) => {
                    self.advance();
                }
                _ => return,
            }
        }
    }

    fn scan_label(&mut self) -> Result<Token, Error> {
        loop {
            match self.advance() {
                Some('"') => {
                    // '"'.len_uft8() == 1
                    let slice = self
                        .source
                        .get(self.start + 1..self.current - 1)
                        .expect("Unfortunate bug...");
                    return Ok(Token::Label(slice.to_owned()));
                }
                Some('\n') => return Err(Error::UnexpectedLineBreak),
                Some(c) if c != '"' => {}
                _ => return Err(Error::UnexpectedEof),
            }
        }
    }

    #[inline]
    fn remainder(&self) -> Option<&str> {
        self.source.get(self.start..)
    }

    #[inline]
    fn peek(&self) -> Option<char> {
        let remaining = self.source.get(self.current..)?;
        remaining.chars().next()
    }
    fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;

        self.current += c.len_utf8();

        Some(c)
    }
}

#[inline]
fn is_white_excluding_newline(c: char) -> bool {
    c.is_whitespace() && c != '\n'
}

#[inline]
fn is_numeric(c: char) -> bool {
    c.is_digit(10) || c == '.'
}
#[inline]
fn is_alpha(c: char) -> bool {
    // a-z, A-Z, 0-9 && !0-9
    c.is_digit(36) && (!c.is_digit(10))
}
#[inline]
fn upper(c: char) -> char {
    c.to_uppercase().next().unwrap()
}

#[cfg(test)]
mod test_utils {
    // LET -> Some(Let)
    pub fn to_debug_name(word: &str) -> String {
        let mut name = "Some(".to_string();
        let chars = word.chars().enumerate().map(|(i, c)| {
            // no unicode issues here, everything is ascii
            if i == 0 {
                c.to_uppercase().next().unwrap()
            } else {
                c.to_lowercase().next().unwrap()
            }
        });
        name.extend(chars);
        name.push(')');

        name
    }
}

#[cfg(test)]
mod test_keyword {
    use super::dfa::*;
    use super::keyword::*;
    use super::test_utils::*;

    #[test]
    fn test_match_keywords() {
        let keywords = vec![
            "LET", "READ", "DATA", "PRINT", "GOTO", "IF", "FOR", "NEXT", "END",
            "DEF", "GOSUB", "RETURN", "DIM", "REM", "TO", "THEN", "STEP",
            "STOP",
        ];

        for keyword in &keywords {
            let mut kw_dfa = KeywordDFA::default();
            let matched = kw_dfa.match_str(keyword).map(|(x, _)| x);
            let matched = format!("{:?}", matched);

            assert_eq!(&matched, &to_debug_name(keyword));
        }
    }
}

#[cfg(test)]
mod test_scanner {
    use super::*;
    use indoc::*;

    fn make_token(line: usize, col: usize, t: Token) -> SourceMapped<Token> {
        SourceMapped {
            value: t,
            loc: SourceLoc { line, col },
        }
    }

    #[test]
    fn test_program() {
        let program = indoc!(
            "
            10 REM POWER TABLE
            11 DATA 8, 4
            15 RE ADN0,P0
            20 PRINT \"N\",
            25 FOR P = 2 to P0
            30   PRINT \"N ^\" P,

            35 NEXT P
            40 PR INT \"SUM\"
            45 LET S = 0
            50 FOR N = 2 TO N0
            55   PRINT N,
            60   FOR P = 2 TO P0
            65     LET S = S + N ^ P
            70     PRINT N ^ P,
            75   NEXT P
            80   PRINT S
            85 NEXT N
            99 END"
        );

        let scanner = Scanner::new(program);
        let tokens = scanner
            .into_iter()
            .filter_map(|r| r.ok())
            .collect::<Vec<_>>();

        let expected = vec![
            make_token(0, 0, Token::Natural(10)),
            make_token(0, 3, Token::Keyword(Keyword::Rem)),
            make_token(0, 18, Token::Eol),
            make_token(1, 0, Token::Natural(11)),
            make_token(1, 3, Token::Keyword(Keyword::Data)),
            make_token(1, 8, Token::Natural(8)),
            make_token(1, 9, Token::Comma),
            make_token(1, 11, Token::Natural(4)),
            make_token(1, 12, Token::Eol),
            make_token(2, 0, Token::Natural(15)),
            make_token(2, 3, Token::Keyword(Keyword::Read)),
            make_token(2, 8, Token::Varname("N0".parse().unwrap())),
            make_token(2, 10, Token::Comma),
            make_token(2, 11, Token::Varname("P0".parse().unwrap())),
            make_token(2, 13, Token::Eol),
            make_token(3, 0, Token::Natural(20)),
            make_token(3, 3, Token::Keyword(Keyword::Print)),
            make_token(3, 9, Token::Label("N".to_string())),
            make_token(3, 12, Token::Comma),
            make_token(3, 13, Token::Eol),
            make_token(4, 0, Token::Natural(25)),
            make_token(4, 3, Token::Keyword(Keyword::For)),
            make_token(4, 7, Token::Varname("P".parse().unwrap())),
            make_token(4, 9, Token::Equal),
            make_token(4, 11, Token::Natural(2)),
            make_token(4, 13, Token::Keyword(Keyword::To)),
            make_token(4, 16, Token::Varname("P0".parse().unwrap())),
            make_token(4, 18, Token::Eol),
            make_token(5, 0, Token::Natural(30)),
            make_token(5, 5, Token::Keyword(Keyword::Print)),
            make_token(5, 11, Token::Label("N ^".to_string())),
            make_token(5, 17, Token::Varname("P".parse().unwrap())),
            make_token(5, 18, Token::Comma),
            make_token(5, 19, Token::Eol),
            make_token(6, 0, Token::Eol),
            make_token(7, 0, Token::Natural(35)),
            make_token(7, 3, Token::Keyword(Keyword::Next)),
            make_token(7, 8, Token::Varname("P".parse().unwrap())),
            make_token(7, 9, Token::Eol),
            make_token(8, 0, Token::Natural(40)),
            make_token(8, 3, Token::Keyword(Keyword::Print)),
            make_token(8, 10, Token::Label("SUM".to_string())),
            make_token(8, 15, Token::Eol),
            make_token(9, 0, Token::Natural(45)),
            make_token(9, 3, Token::Keyword(Keyword::Let)),
            make_token(9, 7, Token::Varname("S".parse().unwrap())),
            make_token(9, 9, Token::Equal),
            make_token(9, 11, Token::Natural(0)),
            make_token(9, 12, Token::Eol),
            make_token(10, 0, Token::Natural(50)),
            make_token(10, 3, Token::Keyword(Keyword::For)),
            make_token(10, 7, Token::Varname("N".parse().unwrap())),
            make_token(10, 9, Token::Equal),
            make_token(10, 11, Token::Natural(2)),
            make_token(10, 13, Token::Keyword(Keyword::To)),
            make_token(10, 16, Token::Varname("N0".parse().unwrap())),
            make_token(10, 18, Token::Eol),
            make_token(11, 0, Token::Natural(55)),
            make_token(11, 5, Token::Keyword(Keyword::Print)),
            make_token(11, 11, Token::Varname("N".parse().unwrap())),
            make_token(11, 12, Token::Comma),
            make_token(11, 13, Token::Eol),
            make_token(12, 0, Token::Natural(60)),
            make_token(12, 5, Token::Keyword(Keyword::For)),
            make_token(12, 9, Token::Varname("P".parse().unwrap())),
            make_token(12, 11, Token::Equal),
            make_token(12, 13, Token::Natural(2)),
            make_token(12, 15, Token::Keyword(Keyword::To)),
            make_token(12, 18, Token::Varname("P0".parse().unwrap())),
            make_token(12, 20, Token::Eol),
            make_token(13, 0, Token::Natural(65)),
            make_token(13, 7, Token::Keyword(Keyword::Let)),
            make_token(13, 11, Token::Varname("S".parse().unwrap())),
            make_token(13, 13, Token::Equal),
            make_token(13, 15, Token::Varname("S".parse().unwrap())),
            make_token(13, 17, Token::Plus),
            make_token(13, 19, Token::Varname("N".parse().unwrap())),
            make_token(13, 21, Token::CaretUp),
            make_token(13, 23, Token::Varname("P".parse().unwrap())),
            make_token(13, 24, Token::Eol),
            make_token(14, 0, Token::Natural(70)),
            make_token(14, 7, Token::Keyword(Keyword::Print)),
            make_token(14, 13, Token::Varname("N".parse().unwrap())),
            make_token(14, 15, Token::CaretUp),
            make_token(14, 17, Token::Varname("P".parse().unwrap())),
            make_token(14, 18, Token::Comma),
            make_token(14, 19, Token::Eol),
            make_token(15, 0, Token::Natural(75)),
            make_token(15, 5, Token::Keyword(Keyword::Next)),
            make_token(15, 10, Token::Varname("P".parse().unwrap())),
            make_token(15, 11, Token::Eol),
            make_token(16, 0, Token::Natural(80)),
            make_token(16, 5, Token::Keyword(Keyword::Print)),
            make_token(16, 11, Token::Varname("S".parse().unwrap())),
            make_token(16, 12, Token::Eol),
            make_token(17, 0, Token::Natural(85)),
            make_token(17, 3, Token::Keyword(Keyword::Next)),
            make_token(17, 8, Token::Varname("N".parse().unwrap())),
            make_token(17, 9, Token::Eol),
            make_token(18, 0, Token::Natural(99)),
            make_token(18, 3, Token::Keyword(Keyword::End)),
            make_token(18, 6, Token::Eof),
        ];

        assert_eq!(&expected, &tokens);
    }
}
