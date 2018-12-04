use std::collections::VecDeque;
use std::f64;

use int_hash::IntHashMap;
use num_traits::FromPrimitive;

use self::value::*;
use crate::ast::function::Func;
use crate::ast::*;

pub mod value;

mod chunk;
mod line_mapping;
mod opcode;

pub use self::chunk::*;
pub use self::opcode::*;

#[derive(Debug)]
pub struct CallFrame {
    depth: usize,
    local: Option<Number>,
    ip: usize,
    func: Option<Func>,
}

pub struct VM {
    chunk: Chunk,
    globals: IntHashMap<Variable, Number>,
    stack: VecDeque<Number>,
    call_stack: VecDeque<CallFrame>,
}

#[derive(Debug)]
pub struct RuntimeError;

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        let mut call_stack = VecDeque::new();
        let call_frame = CallFrame {
            depth: 0,
            local: None,
            ip: 0,
            func: None,
        };
        call_stack.push_back(call_frame);
        VM {
            chunk,
            globals: IntHashMap::default(),
            stack: VecDeque::new(),
            call_stack,
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            let instr = OpCode::from_u8(self.read_byte()?).ok_or(RuntimeError)?;
            match instr {
                OpCode::Noop => continue,
                OpCode::Stop => return Ok(()),
                OpCode::Jump => {
                    let jump_point: JumpPoint = self.read_operand()?;
                    *self.get_ip() = jump_point.0;
                }
                OpCode::CondJump => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    let jump_point: JumpPoint = self.read_operand()?;

                    if value != 0.0 {
                        *self.get_ip() = jump_point.0;
                    }
                }
                OpCode::Constant => {
                    let value = self.read_operand()?;
                    self.push_value(value);
                }
                OpCode::Pop => {
                    self.pop_value().ok_or(RuntimeError)?;
                }
                OpCode::Dup => {
                    let value = self.peek(0).ok_or(RuntimeError)?;
                    self.push_value(value);
                }
                OpCode::Swap => {
                    let v1 = self.pop_value().ok_or(RuntimeError)?;
                    let v2 = self.pop_value().ok_or(RuntimeError)?;
                    self.push_value(v1);
                    self.push_value(v2);
                }
                OpCode::CallNative => {
                    let func: Func = self.read_inline_operand()?;
                    let x = self.pop_value().ok_or(RuntimeError)?;
                    let y = match func {
                        Func::Sin => x.sin(),
                        Func::Cos => x.cos(),
                        Func::Tan => x.tan(),
                        Func::Atn => x.atan(),
                        Func::Exp => x.exp(),
                        Func::Abs => x.abs(),
                        Func::Log => x.ln(),
                        Func::Sqr => x.sqrt(),
                        Func::Rnd => unimplemented!(),
                        Func::Int => x.trunc(),
                        _ => unreachable!("Compiler bug"),
                    };
                    self.push_value(y);
                }
                OpCode::Return => {
                    let v = self.pop_value();
                    self.call_stack.pop_back();
                    if let Some(v) = v {
                        self.push_value(v);
                    }
                }
                OpCode::Call => {
                    let current_depth = self.current_frame().depth;
                    let func: Func = self.read_inline_operand()?;
                    let x = self.pop_value().ok_or(RuntimeError)?;
                    let new_frame = CallFrame {
                        depth: current_depth + 1,
                        func: Some(func),
                        local: Some(x),
                        ip: 0,
                    };
                    self.call_stack.push_back(new_frame);
                }
                OpCode::GetLocal => {
                    let frame = self.current_frame();
                    let x = frame.local.ok_or(RuntimeError)?;
                    self.push_value(x);
                }
                OpCode::GetGlobal => {
                    let var: Variable = self.read_inline_operand()?;
                    let v = self.globals.get(&var).ok_or(RuntimeError)?;
                    self.push_value(*v);
                }
                OpCode::SetGlobal => {
                    let var: Variable = self.read_inline_operand()?;
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    self.globals.insert(var, value);
                }
                OpCode::Negate => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    let neg_value = -value;
                    self.push_value(neg_value);
                }
                OpCode::Sign => {
                    let value = self.pop_value().ok_or(RuntimeError)?;
                    self.push_value(value.signum());
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

    #[inline]
    fn current_frame(&self) -> &CallFrame {
        self.call_stack.back().unwrap()
    }

    #[inline]
    fn current_chunk(&mut self) -> Result<&mut Chunk, RuntimeError> {
        let frame = self.call_stack.back_mut().unwrap();
        match frame.func {
            Some(func) => self.chunk.get_function(&func).ok_or(RuntimeError),
            _ => Ok(&mut self.chunk),
        }
    }

    #[inline]
    fn get_ip(&mut self) -> &mut usize {
        let frame = self.call_stack.back_mut().unwrap();
        &mut frame.ip
    }

    fn read_byte(&mut self) -> Result<u8, RuntimeError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let byte = chunk.read_byte(ip);

        *self.get_ip() += 1;

        Ok(byte)
    }
    fn read_operand<T: Operand>(&mut self) -> Result<T, RuntimeError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let o = chunk.read_operand(ip);
        *self.get_ip() += 2;

        Ok(o)
    }
    fn read_inline_operand<T: InlineOperand>(&mut self) -> Result<T, RuntimeError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let o = chunk.read_inline_operand(ip);
        *self.get_ip() += 2;

        Ok(o)
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
