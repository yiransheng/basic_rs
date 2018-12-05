use std::cmp::Ordering;
use std::ops::Range;

pub struct LineMapping {
    lines: Vec<(usize, Range<usize>)>,
}

impl LineMapping {
    pub fn new() -> Self {
        LineMapping { lines: Vec::new() }
    }
    pub fn add_mapping(&mut self, line: usize, code_point: usize) {
        match self.lines.last_mut() {
            Some((last_line, ref mut rng)) => {
                if line == *last_line {
                    *(&mut rng.end) = code_point;
                } else {
                    self.lines.push((line, code_point - 1..code_point));
                }
            }
            None => {
                self.lines.push((line, code_point - 1..code_point));
            }
        }
    }
    pub fn find_line(&self, code_point: usize) -> usize {
        let idx = match self.lines.binary_search_by(|(_, rng)| {
            if code_point < rng.start {
                Ordering::Less
            } else if code_point >= rng.end {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }) {
            Ok(idx) => idx,
            Err(idx) => idx,
        };

        if idx >= self.lines.len() {
            self.lines.last().unwrap().0
        } else {
            self.lines[idx].0
        }
    }
}
