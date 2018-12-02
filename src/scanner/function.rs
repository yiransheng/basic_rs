use super::dfa::{Dfa, State};
use crate::ast::function::Func;

pub struct FuncDFA {
    state: State<Func>,
}

impl Default for FuncDFA {
    fn default() -> Self {
        FuncDFA {
            state: State::Intermediate(0),
        }
    }
}

impl Dfa for FuncDFA {
    type Output = Func;

    fn match_str(&mut self, s: &str) -> Option<(Func, usize)> {
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

fn _dfa(state: State<Func>, t: char) -> State<Func> {
    match state {
        State::Matched(_) => State::Fail,
        State::Intermediate(0) => _state_0(t),
        State::Intermediate(37) => _state_37(t),
        State::Intermediate(38) => _state_38(t),
        State::Intermediate(39) => _state_39(t),
        State::Intermediate(40) => _state_40(t),
        State::Intermediate(41) => _state_41(t),
        State::Intermediate(42) => _state_42(t),
        State::Intermediate(43) => _state_43(t),
        State::Intermediate(44) => _state_44(t),
        State::Intermediate(45) => _state_45(t),
        State::Intermediate(46) => _state_46(t),
        State::Intermediate(47) => _state_47(t),
        State::Intermediate(48) => _state_48(t),
        State::Intermediate(49) => _state_49(t),
        State::Intermediate(50) => _state_50(t),
        State::Intermediate(51) => _state_51(t),
        State::Intermediate(52) => _state_52(t),
        State::Intermediate(53) => _state_53(t),
        State::Intermediate(54) => _state_54(t),
        State::Intermediate(55) => _state_55(t),
        State::Intermediate(56) => _state_56(t),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_0(t: char) -> State<Func> {
    match t {
        'S' | 's' => State::Intermediate(37),
        'C' | 'c' => State::Intermediate(39),
        'T' | 't' => State::Intermediate(41),
        'A' | 'a' => State::Intermediate(43),
        'E' | 'e' => State::Intermediate(45),
        'L' | 'l' => State::Intermediate(48),
        'R' | 'r' => State::Intermediate(51),
        'I' | 'i' => State::Intermediate(53),
        'F' | 'f' => State::Intermediate(55),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(0),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_37(t: char) -> State<Func> {
    match t {
        'I' | 'i' => State::Intermediate(38),
        'Q' | 'q' => State::Intermediate(50),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(37),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_38(t: char) -> State<Func> {
    match t {
        'N' | 'n' => State::Matched(Func::Sin),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(38),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_39(t: char) -> State<Func> {
    match t {
        'O' | 'o' => State::Intermediate(40),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(39),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_40(t: char) -> State<Func> {
    match t {
        'S' | 's' => State::Matched(Func::Cos),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(40),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_41(t: char) -> State<Func> {
    match t {
        'A' | 'a' => State::Intermediate(42),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(41),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_42(t: char) -> State<Func> {
    match t {
        'N' | 'n' => State::Matched(Func::Tan),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(42),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_43(t: char) -> State<Func> {
    match t {
        'T' | 't' => State::Intermediate(44),
        'B' | 'b' => State::Intermediate(47),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(43),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_44(t: char) -> State<Func> {
    match t {
        'N' | 'n' => State::Matched(Func::Atn),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(44),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_45(t: char) -> State<Func> {
    match t {
        'X' | 'x' => State::Intermediate(46),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(45),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_46(t: char) -> State<Func> {
    match t {
        'P' | 'p' => State::Matched(Func::Exp),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(46),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_47(t: char) -> State<Func> {
    match t {
        'S' | 's' => State::Matched(Func::Abs),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(47),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_48(t: char) -> State<Func> {
    match t {
        'O' | 'o' => State::Intermediate(49),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(48),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_49(t: char) -> State<Func> {
    match t {
        'G' | 'g' => State::Matched(Func::Log),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(49),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_50(t: char) -> State<Func> {
    match t {
        'R' | 'r' => State::Matched(Func::Sqr),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(50),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_51(t: char) -> State<Func> {
    match t {
        'N' | 'n' => State::Intermediate(52),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(51),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_52(t: char) -> State<Func> {
    match t {
        'D' | 'd' => State::Matched(Func::Rnd),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(52),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_53(t: char) -> State<Func> {
    match t {
        'N' | 'n' => State::Intermediate(54),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(53),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_54(t: char) -> State<Func> {
    match t {
        'T' | 't' => State::Matched(Func::Int),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(54),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_55(t: char) -> State<Func> {
    match t {
        'N' | 'n' => State::Intermediate(56),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(55),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_56(t: char) -> State<Func> {
    match t {
        'A' | 'a' => State::Matched(Func::Fna),
        'B' | 'b' => State::Matched(Func::Fnb),
        'C' | 'c' => State::Matched(Func::Fnc),
        'D' | 'd' => State::Matched(Func::Fnd),
        'E' | 'e' => State::Matched(Func::Fne),
        'F' | 'f' => State::Matched(Func::Fnf),
        'G' | 'g' => State::Matched(Func::Fng),
        'H' | 'h' => State::Matched(Func::Fnh),
        'I' | 'i' => State::Matched(Func::Fni),
        'J' | 'j' => State::Matched(Func::Fnj),
        'K' | 'k' => State::Matched(Func::Fnk),
        'L' | 'l' => State::Matched(Func::Fnl),
        'M' | 'm' => State::Matched(Func::Fnm),
        'N' | 'n' => State::Matched(Func::Fnn),
        'O' | 'o' => State::Matched(Func::Fno),
        'P' | 'p' => State::Matched(Func::Fnp),
        'Q' | 'q' => State::Matched(Func::Fnq),
        'R' | 'r' => State::Matched(Func::Fnr),
        'S' | 's' => State::Matched(Func::Fns),
        'T' | 't' => State::Matched(Func::Fnt),
        'U' | 'u' => State::Matched(Func::Fnu),
        'V' | 'v' => State::Matched(Func::Fnv),
        'W' | 'w' => State::Matched(Func::Fnw),
        'X' | 'x' => State::Matched(Func::Fnx),
        'Y' | 'y' => State::Matched(Func::Fny),
        'Z' | 'z' => State::Matched(Func::Fnz),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(56),
        _ => State::Fail,
    }
}
