const fs = require("fs");
const { codegen, typegen } = require("./dfa");

const typePath = "ast/keyword";
const scannerPath = "scanner/keyword";

function toModPath(path) {
  return path.replace(/\//g, "::");
}

function toFilePath(path) {
  return `${__dirname}/${path}.rs`;
}

const keywords = [
  "LET",
  "READ",
  "DATA",
  "PRINT",
  "GOTO",
  "IF",
  "INPUT",
  "FOR",
  "NEXT",
  "END",
  "DEF",
  "GOSUB",
  "RETURN",
  "DIM",
  "REM",
  "TO",
  "THEN",
  "STEP",
  "STOP"
];

const Type = "Keyword";

const typecode =`use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, FromPrimitive, ToPrimitive)]
${typegen(Type, keywords, false)}`;

fs.writeFileSync(toFilePath(typePath), typecode);


// Scanner compiler time DFA

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

${codegen(Type, keywords)}
`;

fs.writeFileSync(toFilePath(scannerPath), scanner);
