use super::dfa::Dfa;

pub struct MatchFloat;

impl Dfa for MatchFloat {
    type Output = f64;

    fn match_str(&mut self, s: &str) -> Option<(Self::Output, usize)> {
        let mut seen_dot = false;
        let mut seen_exp = false;

        let mut consumed: usize = 0;
        let mut prev_char: u8 = 0;

        for c in s.bytes() {
            match c {
                c @ b'0'...b'9' => {
                    prev_char = c;
                    consumed += 1;
                }
                b'.' => {
                    if seen_dot {
                        return None;
                    } else {
                        seen_dot = true;
                        prev_char = b'.';
                        consumed += 1;
                    }
                }
                b'E' | b'e' => {
                    if seen_exp {
                        return None;
                    } else {
                        seen_exp = true;
                        prev_char = b'E';
                        consumed += 1;
                    }
                }
                b'-' => {
                    if prev_char != b'E' {
                        return None;
                    } else {
                        consumed += 1;
                        prev_char = b'-';
                    }
                }
                _ => break,
            }
        }

        let n = s.get(..consumed).and_then(|s| s.parse().ok());

        n.map(|n| (n, consumed))
    }
}
