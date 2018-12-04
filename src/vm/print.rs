use std::io::{Error, Write};
use std::mem;
use std::str::from_utf8_unchecked;

use unicode_width::UnicodeWidthStr;

#[derive(Debug, Copy, Clone)]
enum PrintState {
    Printing(usize),
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
    pub fn write_start(&mut self) {
        match self.state {
            PrintState::Printing(_) => {}
            PrintState::Idle => {
                self.state = PrintState::Printing(0);
            }
        }
    }
    pub fn write_end(&mut self) {
        write!(&mut self.out, "\n");
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
            PrintState::Printing(ref mut written) => {
                let w = UnicodeWidthStr::width(s);
                write!(&mut self.out, "{}", s)?;
                *written += w;
                Ok(())
            }
            _ => Err(PrintError::NotPrinting),
        }
    }

    pub fn advance_to_multiple(&mut self, k: usize) -> Result<(), PrintError> {
        debug_assert!(k > 0);

        match self.state {
            PrintState::Printing(ref mut written) => {
                let n = k - *written % k;
                for _ in 0..n {
                    write!(self.out, " ")?;
                    *written += 1;
                }
                Ok(())
            }
            _ => Err(PrintError::NotPrinting),
        }
    }
}
