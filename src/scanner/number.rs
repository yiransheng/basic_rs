use std::iter::{IntoIterator, Peekable};
use std::str::FromStr;

use super::dfa::Dfa;
use crate::ast::Token;

pub struct MatchNum;

impl Dfa for MatchNum {
    type Output = Token;

    fn match_str(&mut self, s: &str) -> Option<(Self::Output, usize)> {
        match MatchNumLen::new(s.bytes()).real() {
            NumMatch::Natural(n) => {
                let num = s.get(..n).and_then(|s| u64::from_str(s).ok())?;
                Some((Token::Natural(num), n))
            }
            NumMatch::Real(n) => {
                let num = s.get(..n).and_then(|s| f64::from_str(s).ok())?;
                Some((Token::Real(num), n))
            }
            NumMatch::NA => None,
        }
    }
}

// Natural ::= \d+
// Float   ::= Natural | (Natural? "." Natural)
// Real    ::= Float ( ("e" | "E") ("-" | "+")? Natural )?

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum NumMatch {
    Natural(usize),
    Real(usize),
    NA,
}

pub struct MatchNumLen<D> {
    digits: D,
}

macro_rules! try_natural {
    ($e:expr) => {
        match NumMatch::from($e) {
            NumMatch::Natural(n) => n,
            _ => return NumMatch::NA,
        }
    };
}
macro_rules! try_real {
    ($e:expr) => {
        match NumMatch::from($e) {
            NumMatch::Natural(n) => n,
            NumMatch::Real(n) => n,
            _ => return NumMatch::NA,
        }
    };
}

impl<D: Iterator<Item = u8>> MatchNumLen<Peekable<D>> {
    fn new<I: IntoIterator<Item = u8, IntoIter = D>>(digits: I) -> Self {
        MatchNumLen {
            digits: digits.into_iter().peekable(),
        }
    }
    fn real(&mut self) -> NumMatch {
        let num = self.float();
        let n_float = try_real!(num);

        let n_e = match self.digits.next() {
            Some(b'E') | Some(b'e') => 1,
            _ => return num,
        };

        let n_exponent = match self.digits.peek() {
            Some(b'-') | Some(b'+') => {
                self.digits.next();
                let n_power = try_natural!(self.natural());
                n_power + 1
            }
            Some(b'0'...b'9') => try_natural!(self.natural()),
            _ => return num,
        };

        NumMatch::Real(n_float + n_e + n_exponent)
    }

    fn float(&mut self) -> NumMatch {
        let d = match self.digits.peek() {
            Some(d) => *d,
            _ => return NumMatch::NA,
        };

        match d {
            b'.' => {
                self.digits.next();
                let n_frac = try_natural!(self.natural());
                NumMatch::Real(n_frac + 1)
            }
            b'0'...b'9' => {
                let n_int = try_natural!(self.natural());

                if let Some(b'.') = self.digits.peek() {
                    self.digits.next();
                    let n_frac = try_natural!(self.natural());
                    NumMatch::Real(n_int + 1 + n_frac)
                } else {
                    NumMatch::Natural(n_int)
                }
            }
            _ => NumMatch::NA,
        }
    }

    fn natural(&mut self) -> NumMatch {
        let mut consumed = 0;

        while let Some(d) = self.digits.peek() {
            match *d {
                b'0'...b'9' => {
                    self.digits.next();
                    consumed += 1;
                }
                _ => break,
            }
        }

        if consumed > 0 {
            NumMatch::Natural(consumed)
        } else {
            NumMatch::NA
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_match_num_ok() {
        let mut test_cases = vec![
            ("0", NumMatch::Natural(1)),
            ("93", NumMatch::Natural(2)),
            ("0e", NumMatch::Natural(1)),
            ("12E", NumMatch::Natural(2)),
            ("8-9", NumMatch::Natural(1)),
            ("1+1", NumMatch::Natural(1)),
            ("24e.22", NumMatch::Natural(2)),
            ("0099DEF", NumMatch::Natural(4)),
            ("0.0", NumMatch::Real(3)),
            (".338____", NumMatch::Real(4)),
            ("3.14", NumMatch::Real(4)),
            ("0e10", NumMatch::Real(4)),
            ("1.0E10", NumMatch::Real(6)),
            ("33E-10+32", NumMatch::Real(6)),
            (".2E+10E20", NumMatch::Real(6)),
        ];

        for (raw, expected) in test_cases.drain(..) {
            let m = MatchNumLen::new(raw.bytes()).real();

            assert_eq!(expected, m, "<-- input: {}", raw);
        }
    }

    #[test]
    fn test_match_num_err() {
        let mut test_cases = vec![
            ".", "e", "E", "-", "+", " 0", "a32", "12.", "..32", "e23", "-13", "1e-", "2e+LETX=",
            ".e0",
        ];

        for raw in test_cases.drain(..) {
            let m = MatchNumLen::new(raw.bytes()).real();

            assert_eq!(NumMatch::NA, m, "<-- input: {}", raw);
        }
    }
}
