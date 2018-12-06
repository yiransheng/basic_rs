use crate::ast::keyword::Keyword;
use super::dfa::{Dfa, State};

pub struct KeywordDFA {
    state: State<Keyword>,
}

impl Default for KeywordDFA {
    fn default() -> Self {
        KeywordDFA {
            state: State::Intermediate(0),
        }
    }
}

impl Dfa for KeywordDFA {
    type Output = Keyword;

    fn match_str(&mut self, s: &str) -> Option<(Keyword, usize)> {
      
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

fn _dfa(state: State<Keyword>, t: char) -> State<Keyword> {
    match state {
        State::Matched(_) => State::Fail,
        State::Intermediate(0) => _state_0(t),
        State::Intermediate(19) => _state_19(t),
        State::Intermediate(20) => _state_20(t),
        State::Intermediate(21) => _state_21(t),
        State::Intermediate(22) => _state_22(t),
        State::Intermediate(23) => _state_23(t),
        State::Intermediate(24) => _state_24(t),
        State::Intermediate(25) => _state_25(t),
        State::Intermediate(26) => _state_26(t),
        State::Intermediate(27) => _state_27(t),
        State::Intermediate(28) => _state_28(t),
        State::Intermediate(29) => _state_29(t),
        State::Intermediate(30) => _state_30(t),
        State::Intermediate(31) => _state_31(t),
        State::Intermediate(32) => _state_32(t),
        State::Intermediate(33) => _state_33(t),
        State::Intermediate(34) => _state_34(t),
        State::Intermediate(35) => _state_35(t),
        State::Intermediate(36) => _state_36(t),
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
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_0 (t: char) -> State<Keyword> {
    match t {
        'L' | 'l' => State::Intermediate(19),
        'R' | 'r' => State::Intermediate(21),
        'D' | 'd' => State::Intermediate(24),
        'P' | 'p' => State::Intermediate(27),
        'G' | 'g' => State::Intermediate(31),
        'I' | 'i' => State::Intermediate(34),
        'F' | 'f' => State::Intermediate(35),
        'N' | 'n' => State::Intermediate(37),
        'E' | 'e' => State::Intermediate(40),
        'T' | 't' => State::Intermediate(49),
        'S' | 's' => State::Intermediate(52),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(0),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_19 (t: char) -> State<Keyword> {
    match t {
        'E' | 'e' => State::Intermediate(20),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(19),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_20 (t: char) -> State<Keyword> {
    match t {
        'T' | 't' => State::Matched(Keyword::Let),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(20),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_21 (t: char) -> State<Keyword> {
    match t {
        'E' | 'e' => State::Intermediate(22),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(21),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_22 (t: char) -> State<Keyword> {
    match t {
        'A' | 'a' => State::Intermediate(23),
        'T' | 't' => State::Intermediate(45),
        'M' | 'm' => State::Matched(Keyword::Rem),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(22),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_23 (t: char) -> State<Keyword> {
    match t {
        'D' | 'd' => State::Matched(Keyword::Read),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(23),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_24 (t: char) -> State<Keyword> {
    match t {
        'A' | 'a' => State::Intermediate(25),
        'E' | 'e' => State::Intermediate(42),
        'I' | 'i' => State::Intermediate(48),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(24),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_25 (t: char) -> State<Keyword> {
    match t {
        'T' | 't' => State::Intermediate(26),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(25),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_26 (t: char) -> State<Keyword> {
    match t {
        'A' | 'a' => State::Matched(Keyword::Data),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(26),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_27 (t: char) -> State<Keyword> {
    match t {
        'R' | 'r' => State::Intermediate(28),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(27),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_28 (t: char) -> State<Keyword> {
    match t {
        'I' | 'i' => State::Intermediate(29),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(28),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_29 (t: char) -> State<Keyword> {
    match t {
        'N' | 'n' => State::Intermediate(30),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(29),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_30 (t: char) -> State<Keyword> {
    match t {
        'T' | 't' => State::Matched(Keyword::Print),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(30),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_31 (t: char) -> State<Keyword> {
    match t {
        'O' | 'o' => State::Intermediate(32),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(31),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_32 (t: char) -> State<Keyword> {
    match t {
        'T' | 't' => State::Intermediate(33),
        'S' | 's' => State::Intermediate(43),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(32),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_33 (t: char) -> State<Keyword> {
    match t {
        'O' | 'o' => State::Matched(Keyword::Goto),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(33),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_34 (t: char) -> State<Keyword> {
    match t {
        'F' | 'f' => State::Matched(Keyword::If),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(34),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_35 (t: char) -> State<Keyword> {
    match t {
        'O' | 'o' => State::Intermediate(36),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(35),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_36 (t: char) -> State<Keyword> {
    match t {
        'R' | 'r' => State::Matched(Keyword::For),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(36),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_37 (t: char) -> State<Keyword> {
    match t {
        'E' | 'e' => State::Intermediate(38),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(37),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_38 (t: char) -> State<Keyword> {
    match t {
        'X' | 'x' => State::Intermediate(39),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(38),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_39 (t: char) -> State<Keyword> {
    match t {
        'T' | 't' => State::Matched(Keyword::Next),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(39),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_40 (t: char) -> State<Keyword> {
    match t {
        'N' | 'n' => State::Intermediate(41),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(40),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_41 (t: char) -> State<Keyword> {
    match t {
        'D' | 'd' => State::Matched(Keyword::End),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(41),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_42 (t: char) -> State<Keyword> {
    match t {
        'F' | 'f' => State::Matched(Keyword::Def),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(42),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_43 (t: char) -> State<Keyword> {
    match t {
        'U' | 'u' => State::Intermediate(44),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(43),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_44 (t: char) -> State<Keyword> {
    match t {
        'B' | 'b' => State::Matched(Keyword::Gosub),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(44),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_45 (t: char) -> State<Keyword> {
    match t {
        'U' | 'u' => State::Intermediate(46),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(45),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_46 (t: char) -> State<Keyword> {
    match t {
        'R' | 'r' => State::Intermediate(47),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(46),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_47 (t: char) -> State<Keyword> {
    match t {
        'N' | 'n' => State::Matched(Keyword::Return),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(47),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_48 (t: char) -> State<Keyword> {
    match t {
        'M' | 'm' => State::Matched(Keyword::Dim),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(48),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_49 (t: char) -> State<Keyword> {
    match t {
        'O' | 'o' => State::Matched(Keyword::To),
        'H' | 'h' => State::Intermediate(50),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(49),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_50 (t: char) -> State<Keyword> {
    match t {
        'E' | 'e' => State::Intermediate(51),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(50),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_51 (t: char) -> State<Keyword> {
    match t {
        'N' | 'n' => State::Matched(Keyword::Then),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(51),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_52 (t: char) -> State<Keyword> {
    match t {
        'T' | 't' => State::Intermediate(53),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(52),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_53 (t: char) -> State<Keyword> {
    match t {
        'E' | 'e' => State::Intermediate(54),
        'O' | 'o' => State::Intermediate(55),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(53),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_54 (t: char) -> State<Keyword> {
    match t {
        'P' | 'p' => State::Matched(Keyword::Step),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(54),
        _ => State::Fail,
    }
}

#[inline(always)]
fn _state_55 (t: char) -> State<Keyword> {
    match t {
        'P' | 'p' => State::Matched(Keyword::Stop),
        a if a.is_whitespace() && a != '\n' => State::Intermediate(55),
        _ => State::Fail,
    }
}
