use std::collections::VecDeque;

use int_hash::IntHashMap;
use num_traits::FromPrimitive;

use self::value::*;
use crate::ast::*;

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
            let instr = OpCode::from_u8(self.read_byte()).ok_or(RuntimeError)?;
            match instr {
                OpCode::Stop => return Ok(()),
                OpCode::Jump => {
                    let jump_point: JumpPoint = self.read_operand();
                    self.ip = jump_point.0;
                }
                OpCode::CondJump => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    if value != 0.0 {
                        let jump_point: JumpPoint = self.read_operand();
                        self.ip = jump_point.0;
                    }
                }
                OpCode::Constant => {
                    let value = self.read_operand();
                    self.push_value(value);
                }
                OpCode::Pop => {
                    self.pop_value().ok_or(RuntimeError)?;
                }
                OpCode::GetGlobal => {
                    let var: Variable = self.read_inline_operand();
                    let v = self.globals.get(&var).ok_or(RuntimeError)?;
                    self.push_value(*v);
                }
                OpCode::SetGlobal => {
                    let var: Variable = self.read_inline_operand();
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    self.globals.insert(var, value);
                }
                OpCode::Negate => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    let neg_value = -value;
                    self.push_value(neg_value);
                }
                OpCode::Not => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    let not_value = if value == 0.0 { 1.0 } else { 0.0 };
                    self.push_value(not_value);
                }
                OpCode::Add => {
                    let value = self.binary_op(|a, b| Some(a + b)).ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OpCode::Sub => {
                    let value = self.binary_op(|a, b| Some(a - b)).ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OpCode::Mul => {
                    let value = self.binary_op(|a, b| Some(a * b)).ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OpCode::Div => {
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
                    self.push_value(value);
                }
                OpCode::Equal => {
                    let value = self
                        .binary_op(|a, b| Some(if a == b { 1.0 } else { 0.0 }))
                        .ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OpCode::Less => {
                    let value = self
                        .binary_op(|a, b| Some(if a < b { 1.0 } else { 0.0 }))
                        .ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OpCode::Greater => {
                    let value = self
                        .binary_op(|a, b| Some(if a > b { 1.0 } else { 0.0 }))
                        .ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OpCode::Print => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    self.print_value(value);
                }
                _ => break,
            }
        }

        Ok(())
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.read_byte(self.ip);

        self.ip += 1;

        byte
    }
    fn read_operand<T: Operand>(&mut self) -> T {
        let o = self.chunk.read_operand(self.ip);
        self.ip += 2;

        o
    }
    fn read_inline_operand<T: InlineOperand>(&mut self) -> T {
        let o = self.chunk.read_inline_operand(self.ip);
        self.ip += 2;

        o
    }

    fn push_value(&mut self, v: Number) {
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
