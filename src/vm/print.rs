use std::io::{BufWriter, Error, Write};
use std::mem;
use std::str::from_utf8_unchecked;

use unicode_width::UnicodeWidthStr;

#[derive(Debug, Copy, Clone)]
enum PrintState {
    Printing(usize, bool),
    Idle,
}

pub enum PrintError {
    Io(Error),
    NotPrinting,
}

impl From<Error> for PrintError {
    fn from(err: Error) -> Self {
        PrintError::Io(err)
    }
}

pub struct Printer<W> {
    num: Vec<u8>,
    out: W,
    state: PrintState,
}

impl<W: Write> Printer<W> {
    pub fn new(out: W) -> Self {
        Printer {
            num: Vec::new(),
            out,
            state: PrintState::Idle,
        }
    }
    pub fn new_buffered(out: W) -> Printer<BufWriter<W>> {
        Printer {
            num: Vec::new(),
            out: BufWriter::with_capacity(100, out),
            state: PrintState::Idle,
        }
    }
    pub fn write_start(&mut self) {
        match self.state {
            PrintState::Printing(..) => {}
            PrintState::Idle => {
                self.state = PrintState::Printing(0, true);
            }
        }
    }
    pub fn write_end(&mut self) {
        match self.state {
            PrintState::Printing(_, true) => {
                write!(&mut self.out, "\n");
            }
            _ => {}
        }
        self.state = PrintState::Idle;
    }
    pub fn write_num(&mut self, n: f64) -> Result<(), PrintError> {
        let mut num = mem::replace(&mut self.num, Vec::new());
        num.clear();
        write!(&mut num, "{}", n)?;
        let s = unsafe { from_utf8_unchecked(&num) };
        let r = self.write_str(s);

        self.num = num;
        r
    }

    pub fn write_str(&mut self, s: &str) -> Result<(), PrintError> {
        debug_assert!(!s.contains('\n'));

        match self.state {
            PrintState::Printing(ref mut written, ref mut end_line) => {
                let w = UnicodeWidthStr::width(s);
                write!(&mut self.out, "{}", s)?;
                *written += w;
                *end_line = true;
                Ok(())
            }
            _ => Err(PrintError::NotPrinting),
        }
    }

    #[inline(always)]
    pub fn advance_to_multiple(&mut self, k: usize) -> Result<(), PrintError> {
        debug_assert!(k > 0);

        match self.state {
            PrintState::Printing(ref mut written, ref mut end_line) => {
                let n = k - *written % k;
                *written += n;
                for _ in 0..n {
                    write!(self.out, " ")?;
                }
                *end_line = false;
                Ok(())
            }
            _ => Err(PrintError::NotPrinting),
        }
    }
}
