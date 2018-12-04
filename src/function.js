const fs = require("fs");
const { codegen, typegen } = require("./dfa");

const typePath = "ast/function";
const scannerPath = "scanner/function";

function toModPath(path) {
  return path.replace(/\//g, "::");
}

function toFilePath(path) {
  return `${__dirname}/${path}.rs`;
}

const funcnames = [
  "SIN",
  "COS",
  "TAN",
  "ATN",
  "EXP",
  "ABS",
  "LOG",
  "SQR",
  "RND",
  "INT",
  "FNA",
  "FNB",
  "FNC",
  "FND",
  "FNE",
  "FNF",
  "FNG",
  "FNH",
  "FNI",
  "FNJ",
  "FNK",
  "FNL",
  "FNM",
  "FNN",
  "FNO",
  "FNP",
  "FNQ",
  "FNR",
  "FNS",
  "FNT",
  "FNU",
  "FNV",
  "FNW",
  "FNX",
  "FNY",
  "FNZ"
];

const Type = "Func";

const typecode =`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
${typegen(Type, funcnames, false)}

impl ${Type} {
    pub fn is_native(&self) -> bool {
        match self {
            ${Type}::Sin => true,
            ${Type}::Cos => true,
            ${Type}::Tan => true,
            ${Type}::Atn => true,
            ${Type}::Exp => true,
            ${Type}::Abs => true,
            ${Type}::Log => true,
            ${Type}::Sqr => true,
            ${Type}::Rnd => true,
            ${Type}::Int => true,
            _ => false,
        }
    }
}`;

fs.writeFileSync(toFilePath(typePath), typecode);

const scanner = `use crate::${toModPath(typePath)}::${Type};
use super::dfa::{Dfa, State};

pub struct ${Type}DFA {
    state: State<${Type}>,
}

impl Default for ${Type}DFA {
    fn default() -> Self {
        ${Type}DFA {
            state: State::Intermediate(0),
        }
    }
}

impl Dfa for ${Type}DFA {
    type Output = ${Type};

    fn match_str(&mut self, s: &str) -> Option<(${Type}, usize)> {
      
        let mut consumed: usize = 0;
        let iter = s.chars();

        for c in iter {
            let next_state = _dfa(self.state, c);
            consumed += c.len_utf8();

            match next_state {
                State::Matched(x) => return Some((x, consumed)),
                State::Fail => return None,
                s @ _ => {
                    self.state = s;
                }
            }
        }

        None
    }
}

${codegen(Type, funcnames)}
`;

fs.writeFileSync(toFilePath(scannerPath), scanner);
