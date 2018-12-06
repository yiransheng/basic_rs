use std::cmp::Ordering;
use std::ops::Range;

#[derive(Debug)]
pub struct LineMapping {
    lines: Vec<(usize, Range<usize>)>,
}

impl LineMapping {
    pub fn new() -> Self {
        LineMapping { lines: Vec::new() }
    }
    pub fn add_mapping(&mut self, line: usize, code_point: usize) {
        let start = match self.lines.last_mut() {
            Some((last_line, ref mut rng)) if *last_line == line => {
                *(&mut rng.end) = code_point;
                return;
            }
            Some((_, rng)) => rng.end,
            None => 0,
        };

        self.lines.push((line, start..code_point))
    }
    pub fn find_line(&self, code_point: usize) -> usize {
        let idx = match self.lines.binary_search_by(|(_, rng)| {
            if code_point < rng.start {
                Ordering::Greater
            } else if code_point >= rng.end {
                Ordering::Less
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
