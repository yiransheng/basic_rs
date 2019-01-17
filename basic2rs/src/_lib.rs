use std::collections::HashMap;
use std::io::{BufRead, BufWriter, Error, Write};
use std::mem;
use std::str::from_utf8_unchecked;

use unicode_width::UnicodeWidthStr;

pub fn exit_io_error<T>(err: Error) -> T {
    eprintln!("{}", err);
    ::std::process::exit(1)
}

pub struct Printer<W> {
    num: Vec<u8>,
    out: W,
    col: usize,
    input_string: String,
}

impl<W: Write> Printer<W> {
    pub fn new_buffered(out: W) -> Printer<BufWriter<W>> {
        Printer {
            num: Vec::new(),
            out: BufWriter::with_capacity(100, out),
            col: 0,
            input_string: String::new(),
        }
    }
    pub fn write_str_(&mut self, s: &str) {
        self.write_str(s).unwrap_or_else(exit_io_error);
    }
    pub fn write_num_(&mut self, n: f64) {
        self.write_num(n).unwrap_or_else(exit_io_error);
    }
    pub fn advance_to_multiple_(&mut self, k: usize) {
        self.advance_to_multiple(k).unwrap_or_else(exit_io_error);
    }
    pub fn writeln_(&mut self) {
        self.writeln().unwrap_or_else(exit_io_error);
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

    pub fn input<R: BufRead>(&mut self, mut inp: R) -> f64 {
        self.flush().unwrap_or_else(exit_io_error);
        self.input_string.clear();

        inp.read_line(&mut self.input_string)
            .unwrap_or_else(exit_io_error);

        let s = (&self.input_string).trim();
        let v: f64 = if s.is_empty() {
            0.0
        } else {
            s.parse().unwrap_or_else(|_| {
                self.write_str_("Invalid INPUT");
                self.writeln_();

                self.input(inp)
            })
        };

        v
    }

    fn flush(&mut self) -> Result<(), Error> {
        self.out.flush()?;

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
