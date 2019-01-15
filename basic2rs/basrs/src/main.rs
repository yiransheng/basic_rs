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
fn main() {
use std::io;

        let stdout = io::stdout();
        let stdin = io::stdin();
    
let mut printer = Printer::new_buffered(stdout.lock());
let mut data: Vec<f64> = vec![
4f64, 
8f64, 
];
let mut __main__ = || {
let mut __label__: usize;
let mut local_0: f64;
let mut local_1: f64;
let mut local_2: f64;
let mut local_3: f64;
let mut local_4: f64;
let mut local_5: f64;
let mut local_6: f64;
let mut local_7: f64;
local_3 = 0f64;
local_4 = 0f64;
local_5 = 0f64;
local_6 = 0f64;
local_7 = 0f64;
local_3 = data.pop().expect("no data");
local_6 = data.pop().expect("no data");
printer.write_str("N");
printer.advance_to_multiple(15);
local_0 = local_6;
local_7 = 2f64;
'a2: loop {
if (( ( local_7-local_0 )>0f64 )) {
__label__ = 5;
break 'a2;
}
else {
__label__ = 3;
}
printer.write_str("N ^");
printer.write_num(local_7);
printer.advance_to_multiple(15);
local_7 = ( local_7+1f64 );
__label__ = 2;
continue 'a2;
}
printer.write_str("SUM");
printer.writeln();
local_5 = 0f64;
local_1 = local_3;
local_4 = 2f64;
'a8: loop {
if (( ( local_4-local_1 )>0f64 )) {
__label__ = 15;
break 'a8;
}
else {
__label__ = 8;
}
printer.write_num(local_4);
printer.advance_to_multiple(15);
local_2 = local_6;
local_7 = 2f64;
'a12: loop {
if (( ( local_7-local_2 )>0f64 )) {
__label__ = 13;
break 'a12;
}
else {
__label__ = 11;
}
local_5 = ( local_5+local_4.powf( local_7 ) );
printer.write_num(local_4.powf( local_7 ));
printer.advance_to_multiple(15);
local_7 = ( local_7+1f64 );
__label__ = 10;
continue 'a12;
}
printer.write_num(local_5);
printer.writeln();
local_4 = ( local_4+1f64 );
__label__ = 7;
continue 'a8;
}
return;
};
__main__();
}
