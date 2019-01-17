use std::collections::HashMap;
use std::error;
use std::fmt;
use std::io::{BufWriter, Error, Write};
use std::mem;
use std::str::from_utf8_unchecked;

use unicode_width::UnicodeWidthStr;

#[derive(Debug)]
pub enum PrintError {
    Io(Error),
}

impl From<Error> for PrintError {
    fn from(err: Error) -> Self {
        PrintError::Io(err)
    }
}

impl fmt::Display for PrintError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrintError::Io(err) => err.fmt(formatter),
        }
    }
}

impl error::Error for PrintError {
    fn description(&self) -> &str {
        match self {
            PrintError::Io(err) => err.description(),
        }
    }
}

pub struct Printer<W> {
    num: Vec<u8>,
    out: W,
    col: usize,
}

impl<W: Write> Printer<W> {
    #[allow(dead_code)]
    pub fn new(out: W) -> Self {
        Printer {
            num: Vec::new(),
            out,
            col: 0,
        }
    }
    pub fn new_buffered(out: W) -> Printer<BufWriter<W>> {
        Printer {
            num: Vec::new(),
            out: BufWriter::with_capacity(100, out),
            col: 0,
        }
    }
    pub fn flush(&mut self) -> Result<(), PrintError> {
        self.out.flush()?;

        Ok(())
    }
    pub fn write_num(&mut self, n: f64) -> Result<(), PrintError> {
        let mut num = mem::replace(&mut self.num, Vec::new());
        num.clear();
        write!(&mut num, "{}", n as f32)?;
        let s = unsafe { from_utf8_unchecked(&num) };
        let r = self.write_str(s);

        self.num = num;
        r
    }

    pub fn write_str(&mut self, s: &str) -> Result<(), PrintError> {
        debug_assert!(!s.contains('\n'));

        let w = UnicodeWidthStr::width(s);
        self.col += w;
        write!(self.out, "{}", s)?;

        Ok(())
    }
    pub fn writeln(&mut self) -> Result<(), PrintError> {
        writeln!(self.out)?;
        self.col = 0;

        Ok(())
    }

    pub fn advance_to_multiple(&mut self, k: usize) -> Result<(), PrintError> {
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
fn copysign(a: f64, b: f64) -> f64 {
    a * b.signum()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Rank {
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
struct SparseArray {
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
    fn mut_1d(&mut self, i: f64) -> &mut f64 {
        self.assert_rank(Rank::One);
        validate_index!(i);

        let key = SparseArray::hash1d(i);

        if !self.data.contains_key(&key) {
            self.data.insert(key, 0.0);
        }

        self.data.get_mut(&key).unwrap()
    }
    fn mut_2d(&mut self, i: f64, j: f64) -> &mut f64 {
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
