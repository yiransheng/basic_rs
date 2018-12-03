use crate::ast::*;
use int_hash::IntHashMap;
use std::collections::VecDeque;

use self::value::*;

pub mod value;

mod chunk;
mod line_mapping;
mod opcode;

pub use self::chunk::*;
pub use self::opcode::*;

pub struct VM {
    chunk: Chunk,
    globals: IntHashMap<Variable, Number>,
    ip: usize,
    stack: VecDeque<Number>,
}

#[derive(Debug)]
pub struct RuntimeError;

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        VM {
            chunk,
            globals: IntHashMap::default(),
            ip: 0,
            stack: VecDeque::new(),
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            let instr: u8 = self.read_byte();
            match instr {
                OP_STOP => return Ok(()),
                OP_PRINT => {
                    if let Some(v) = self.pop_value() {
                        self.print_value(v);
                    }
                }
                OP_CONSTANT => {
                    let constant = self.read_constant();
                    match constant {
                        Variant::Number(n) => {
                            self.push_value(n);
                        }
                        _ => return Err(RuntimeError),
                    }
                }
                OP_POP => {
                    let _ = self.pop_value().ok_or(RuntimeError)?;
                }
                OP_GET_GLOBAL => {
                    let constant = self.read_constant();
                    match constant {
                        Variant::Varname(v) => {
                            let var = Variable::from_bytes_unchecked(v);
                            let v = self.globals.get(&var).ok_or(RuntimeError)?;
                            self.push_value(*v);
                        }
                        _ => return Err(RuntimeError),
                    }
                }
                OP_SET_GLOBAL => {
                    let constant = self.read_constant();
                    match constant {
                        Variant::Varname(v) => {
                            let var = Variable::from_bytes_unchecked(v);
                            let value = self.pop_value().ok_or(RuntimeError)?;
                            self.globals.insert(var, value);
                        }
                        _ => return Err(RuntimeError),
                    }
                }
                OP_NEGATE => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    let neg_value = -value;
                    self.push_value(neg_value);
                }
                OP_ADD => {
                    let value = self.binary_op(|a, b| Some(a + b)).ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OP_SUBTRACT => {
                    let value = self.binary_op(|a, b| Some(a - b)).ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OP_MULTIPLY => {
                    let value = self.binary_op(|a, b| Some(a * b)).ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OP_DIVIDE => {
                    let value = self
                        .binary_op(|a, b| {
                            let v = a / b;
                            if !v.is_nan() {
                                Some(v)
                            } else {
                                None
                            }
                        })
                        .ok_or(RuntimeError)?;
                }
                OP_PRINT => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    self.print_value(value);
                }
                _ => return Err(RuntimeError),
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.read_byte(self.ip);

        self.ip += 1;

        byte
    }
    fn read_constant(&mut self) -> Variant {
        let constant = self.chunk.read_constant(self.ip);

        self.ip += 1;

        constant
    }

    fn push_value(&mut self, v: Number) {
        // panic on overflow
        self.stack.push_back(v);
    }
    fn pop_value(&mut self) -> Option<Number> {
        self.stack.pop_back()
    }
    fn peek(&self, distance: usize) -> Option<Number> {
        let n = self.stack.len();
        let index = n - 1 - distance;

        self.stack.get(index).cloned()
    }

    fn print_value(&mut self, v: Number) {
        println!("{}", v);
    }

    fn binary_op<F>(&mut self, f: F) -> Option<Number>
    where
        F: for<'b> Fn(Number, Number) -> Option<Number>,
    {
        let a = self.pop_value()?;
        let b = self.pop_value()?;

        f(b, a)
    }
}
