mod dfa;
mod function;
mod keyword;
mod number;

use std::iter::IntoIterator;

use crate::ast::keyword::Keyword;
use crate::ast::{NameError, Token, Variable};

use self::dfa::Dfa;
use self::function::FuncDFA;
use self::keyword::KeywordDFA;
use self::number::MatchFloat;

#[derive(Debug, Copy, Clone)]
pub enum Error {
    UnexpectedChar(char),
    UnexpectedLineBreak,
    UnexpectedEof,
    BadNumber,
    BadIdentifier(Option<NameError>),
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
    MatchFloat
        .map(|n| Token::Number(n))
        .match_str(s)
        .ok_or(Error::BadNumber)
}

#[derive(Debug, Copy, Clone)]
pub struct SourceMapped<T> {
    pub value: T,
    pub loc: SourceLoc,
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

#[derive(Debug, Copy, Clone)]
pub struct SourceLoc {
    pub line: usize,
    pub col: usize,
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
            Some(Ok(ref t)) => match &t.value {
                Token::Eof => self.inner = None,
                _ => {}
            },
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

        SourceMapped { value: value, loc }
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
            c @ _ => return Err(Error::UnexpectedChar(c)),
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
            "LET", "READ", "DATA", "PRINT", "GOTO", "IF", "FOR", "NEXT", "END", "DEF", "GOSUB",
            "RETURN", "DIM", "REM", "TO", "THEN", "STEP", "STOP",
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

        let mut scanner = Scanner::new(program);
        let tokens = scanner
            .into_iter()
            .filter_map(|r| r.ok())
            .map(|t| t.value.ty())
            .collect::<Vec<_>>();

        let expected_types = [
            "Number", "Keyword", "Eol", "Number", "Keyword", "Number", "Comma", "Number", "Eol",
            "Number", "Keyword", "Varname", "Comma", "Varname", "Eol", "Number", "Keyword",
            "Label", "Comma", "Eol", "Number", "Keyword", "Varname", "Equal", "Number", "Keyword",
            "Varname", "Eol", "Number", "Keyword", "Label", "Varname", "Comma", "Eol", "Eol",
            "Number", "Keyword", "Varname", "Eol", "Number", "Keyword", "Label", "Eol", "Number",
            "Keyword", "Varname", "Equal", "Number", "Eol", "Number", "Keyword", "Varname",
            "Equal", "Number", "Keyword", "Varname", "Eol", "Number", "Keyword", "Varname",
            "Comma", "Eol", "Number", "Keyword", "Varname", "Equal", "Number", "Keyword",
            "Varname", "Eol", "Number", "Keyword", "Varname", "Equal", "Varname", "Plus",
            "Varname", "CaretUp", "Varname", "Eol", "Number", "Keyword", "Varname", "CaretUp",
            "Varname", "Comma", "Eol", "Number", "Keyword", "Varname", "Eol", "Number", "Keyword",
            "Varname", "Eol", "Number", "Keyword", "Varname", "Eol", "Number", "Keyword", "Eof",
        ];

        assert_eq!(&tokens[..], &expected_types[..]);
    }
}
