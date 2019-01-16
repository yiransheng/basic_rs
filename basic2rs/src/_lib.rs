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

#[derive(Default)]
struct SparseArray {
    data: HashMap<usize, f64>,
}

impl SparseArray {
    fn mut_1d(&mut self, i: f64) -> &mut f64 {
        let key = SparseArray::hash1d(i);

        let key = SparseArray::hash1d(i);
        if !self.data.contains_key(&key) {
            self.data.insert(key, 0.0);
        }

        self.data.get_mut(&key).unwrap()
    }
    fn mut_2d(&mut self, i: f64, j: f64) -> &mut f64 {
        let key = SparseArray::hash2d(i, j);

        let key = SparseArray::hash1d(i);
        if !self.data.contains_key(&key) {
            self.data.insert(key, 0.0);
        }

        self.data.get_mut(&key).unwrap()
    }

    fn hash1d(i: f64) -> usize {
        SparseArray::hash2d(i, 0.0)
    }
    fn hash2d(i: f64, j: f64) -> usize {
        let i = i.trunc() as usize;
        let j = j.trunc() as usize;

        (i + j) * (i + j + 1) / 2 + j
    }
}
