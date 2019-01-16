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
#[derive(Default)]
struct Env {
    I: f64,
    array_B: SparseArray,
    M: f64,
    array_A: SparseArray,
    Y: f64,
    X: f64,
    array_L: SparseArray,
}
fn main() {
    use std::io;

    let mut env = Env::default();
    let stdout = io::stdout();
    let stdin = io::stdin();

    let mut printer = Printer::new_buffered(stdout.lock());
    let mut data: Vec<f64> = vec![
        1f64, 1f64, 1f64, 1f64, 1f64, 1f64, 1f64, 1f64, 1f64, 1f64, 10f64,
        10f64,
    ];
    let mut __fn_0__ = |env: &mut Env| {
        let mut __label__: usize;
        let mut local_0: f64;
        let mut local_1: f64;
        printer.write_str("GEN ");
        printer.write_num(env.I);
        printer.writeln();
        local_0 = env.M;
        env.Y = 1f64;
        'a2: loop {
            if ((env.Y - local_0) > 0f64) {
                __label__ = 11;
                break 'a2;
            } else {
                __label__ = 3;
            }
            local_1 = env.M;
            env.X = 1f64;
            'a5: loop {
                if ((env.X - local_1) > 0f64) {
                    __label__ = 9;
                    break 'a5;
                } else {
                    __label__ = 5;
                }
                if (*env.array_A.mut_2d(env.X, env.Y) == 1f64) {
                    __label__ = 8;
                } else {
                    __label__ = 6;
                }
                'a8: loop {
                    {
                        if (__label__ == 6) {
                            printer.write_str(".");
                            printer.advance_to_multiple(3);
                            __label__ = 7;
                            break 'a8;
                        } else if (__label__ == 8) {
                            printer.write_str("O");
                            printer.advance_to_multiple(3);
                            __label__ = 7;
                            break 'a8;
                        }
                    }
                    break;
                }
                env.X = (env.X + 1f64);
                __label__ = 4;
                continue 'a5;
            }
            printer.writeln();
            env.Y = (env.Y + 1f64);
            __label__ = 2;
            continue 'a2;
        }
        return;
    };
    let mut __fn_1__ = |env: &mut Env| {
        let mut __label__: usize;
        let mut local_0: f64;
        let mut local_1: f64;
        let mut local_2: f64;
        local_2 = 0f64;
        local_0 = env.M;
        env.Y = 1f64;
        'a2: loop {
            if ((env.Y - local_0) > 0f64) {
                __label__ = 8;
                break 'a2;
            } else {
                __label__ = 3;
            }
            local_1 = env.M;
            env.X = 1f64;
            'a5: loop {
                if ((env.X - local_1) > 0f64) {
                    __label__ = 7;
                    break 'a5;
                } else {
                    __label__ = 5;
                }
                local_2 = (((((((*env
                    .array_A
                    .mut_2d((env.X - 1f64), env.Y)
                    + *env.array_A.mut_2d((env.X + 1f64), env.Y))
                    + *env.array_A.mut_2d(env.X, (env.Y - 1f64)))
                    + *env.array_A.mut_2d(env.X, (env.Y + 1f64)))
                    + *env.array_A.mut_2d((env.X - 1f64), (env.Y - 1f64)))
                    + *env.array_A.mut_2d((env.X + 1f64), (env.Y + 1f64)))
                    + *env.array_A.mut_2d((env.X - 1f64), (env.Y + 1f64)))
                    + *env.array_A.mut_2d((env.X + 1f64), (env.Y - 1f64)));
                *env.array_B.mut_2d(env.X, env.Y) = *env
                    .array_L
                    .mut_2d(*env.array_A.mut_2d(env.X, env.Y), local_2);
                env.X = (env.X + 1f64);
                __label__ = 4;
                continue 'a5;
            }
            env.Y = (env.Y + 1f64);
            __label__ = 2;
            continue 'a2;
        }
        return;
    };
    let mut __fn_2__ = |env: &mut Env| {
        let mut __label__: usize;
        let mut local_0: f64;
        let mut local_1: f64;
        local_0 = env.M;
        env.Y = 1f64;
        'a2: loop {
            if ((env.Y - local_0) > 0f64) {
                __label__ = 8;
                break 'a2;
            } else {
                __label__ = 3;
            }
            local_1 = env.M;
            env.X = 1f64;
            'a5: loop {
                if ((env.X - local_1) > 0f64) {
                    __label__ = 7;
                    break 'a5;
                } else {
                    __label__ = 5;
                }
                *env.array_A.mut_2d(env.X, env.Y) =
                    *env.array_B.mut_2d(env.X, env.Y);
                env.X = (env.X + 1f64);
                __label__ = 4;
                continue 'a5;
            }
            env.Y = (env.Y + 1f64);
            __label__ = 2;
            continue 'a2;
        }
        return;
    };
    let mut __main__ = |env: &mut Env| {
        let mut __label__: usize;
        let mut local_0: f64;
        let mut local_1: f64;
        local_1 = 0f64;
;
        local_1 = data.pop().expect("no data");
        env.M = data.pop().expect("no data");
        *env.array_L.mut_2d(0f64, 3f64) = data.pop().expect("no data");
        *env.array_L.mut_2d(1f64, 3f64) = data.pop().expect("no data");
        *env.array_L.mut_2d(1f64, 2f64) = data.pop().expect("no data");
        *env.array_A.mut_2d(3f64, 4f64) = data.pop().expect("no data");
        *env.array_A.mut_2d(3f64, 5f64) = data.pop().expect("no data");
        *env.array_A.mut_2d(3f64, 6f64) = data.pop().expect("no data");
        *env.array_A.mut_2d(6f64, 5f64) = data.pop().expect("no data");
        *env.array_A.mut_2d(6f64, 6f64) = data.pop().expect("no data");
        *env.array_A.mut_2d(7f64, 5f64) = data.pop().expect("no data");
        *env.array_A.mut_2d(7f64, 6f64) = data.pop().expect("no data");
        env.I = 0f64;
        __fn_0__(env);
        local_0 = local_1;
        env.I = 1f64;
        'a2: loop {
            if ((env.I - local_0) > 0f64) {
                __label__ = 5;
                break 'a2;
            } else {
                __label__ = 3;
            }
            __fn_1__(env);
            __fn_2__(env);
            __fn_0__(env);
            env.I = (env.I + 1f64);
            __label__ = 2;
            continue 'a2;
        }
        return;
    };
    __main__(&mut env);
}
