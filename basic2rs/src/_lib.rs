use std::collections::HashMap;
use std::io::{BufWriter, Error, Write};
use std::mem;
use std::str::from_utf8_unchecked;

use unicode_width::UnicodeWidthStr;

pub struct Printer<W> {
    num: Vec<u8>,
    out: W,
    col: usize,
}

impl<W: Write> Printer<W> {
    pub fn new_buffered(out: W) -> Printer<BufWriter<W>> {
        Printer {
            num: Vec::new(),
            out: BufWriter::with_capacity(100, out),
            col: 0,
        }
    }
    pub fn flush(&mut self) -> Result<(), Error> {
        self.out.flush()?;

        Ok(())
    }
    pub fn write_num(&mut self, n: f64) -> Result<(), Error> {
        let mut num = mem::replace(&mut self.num, Vec::new());
        num.clear();
        write!(&mut num, "{}", n as f32)?;
        let s = unsafe { from_utf8_unchecked(&num) };
        let r = self.write_str(s);

        self.num = num;
        r
    }

    pub fn write_str(&mut self, s: &str) -> Result<(), Error> {
        debug_assert!(!s.contains('\n'));

        let w = UnicodeWidthStr::width(s);
        self.col += w;
        write!(self.out, "{}", s)?;

        Ok(())
    }
    pub fn writeln(&mut self) -> Result<(), Error> {
        writeln!(self.out)?;
        self.col = 0;

        Ok(())
    }

    pub fn advance_to_multiple(&mut self, k: usize) -> Result<(), Error> {
        debug_assert!(k > 0);
        let mut written = self.col;
        let n = k - written % k;
        written += n;
        for _ in 0..n {
            write!(self.out, " ")?;
        }
        self.col = written;

        Ok(())
    }
}

#[inline(always)]
pub fn copysign(a: f64, b: f64) -> f64 {
    a * b.signum()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Rank {
    One,
    Two,
    Unknown,
}
impl Default for Rank {
    fn default() -> Self {
        Rank::Unknown
    }
}

#[derive(Default, Debug)]
pub struct SparseArray {
    data: HashMap<[i32; 2], f64>,
    rank: Rank,
}

macro_rules! validate_index {
    ($i:expr) => {
        assert!($i >= 0f64 && $i == $i.trunc());
    };
    ($i:expr, $j: expr) => {
        validate_index!($i);
        validate_index!($j);
    };
}

impl SparseArray {
    pub fn mut_1d(&mut self, i: f64) -> &mut f64 {
        self.assert_rank(Rank::One);
        validate_index!(i);

        let key = SparseArray::hash1d(i);

        if !self.data.contains_key(&key) {
            self.data.insert(key, 0.0);
        }

        self.data.get_mut(&key).unwrap()
    }
    pub fn mut_2d(&mut self, i: f64, j: f64) -> &mut f64 {
        self.assert_rank(Rank::Two);
        validate_index!(i, j);

        let key = SparseArray::hash2d(i, j);

        if !self.data.contains_key(&key) {
            self.data.insert(key, 0.0);
        }

        self.data.get_mut(&key).unwrap()
    }

    #[inline]
    fn assert_rank(&mut self, rank: Rank) {
        match self.rank {
            Rank::Unknown => {
                self.rank = rank;
                return;
            }
            r if r != rank => {
                panic!("Runtime Error: incorrect List/Table access");
            }
            _ => {}
        }
    }

    fn hash1d(i: f64) -> [i32; 2] {
        SparseArray::hash2d(i, -1.0)
    }
    fn hash2d(i: f64, j: f64) -> [i32; 2] {
        let i = i.trunc() as i32;
        let j = j.trunc() as i32;

        [i, j]
    }
}
