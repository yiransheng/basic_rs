use std::collections::VecDeque;
use std::f64;
use std::io;

use int_hash::IntHashMap;
use num_traits::{FromPrimitive, ToPrimitive};

use crate::ast::function::Func;
use crate::ast::*;

mod array;
mod chunk;
mod line_mapping;
mod opcode;
mod print;
mod value;

pub use self::chunk::*;
pub use self::opcode::*;

use self::array::{Array, Error as ArrayError, Subscript};
use self::print::{PrintError, Printer};
use self::value::*;

pub const DEFAULT_ARRAY_SIZE: u8 = 11;

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
    global_lists: IntHashMap<Variable, Array<u8>>,
    global_tables: IntHashMap<Variable, Array<[u8; 2]>>,
    stack: VecDeque<Value>,
    call_stack: VecDeque<CallFrame>,
}

#[derive(Debug)]
pub struct RuntimeError {
    error: ExecError,
    line_no: usize,
}

#[derive(Debug)]
pub enum ExecError {
    NoData,
    EmptyStack,
    ListNotFound(Variable),
    TableNotFound(Variable),
    FunctionNotFound(Func),
    IndexError(Variable, f64),
    TypeError(&'static str),
    ArrayError(ArrayError),
    PrintError(PrintError),
    DecodeError(u8),
    RedefineDim(Variable),
}

impl From<ArrayError> for ExecError {
    fn from(err: ArrayError) -> ExecError {
        ExecError::ArrayError(err)
    }
}

impl From<PrintError> for ExecError {
    fn from(err: PrintError) -> ExecError {
        ExecError::PrintError(err)
    }
}

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
            global_lists: IntHashMap::default(),
            global_tables: IntHashMap::default(),
            stack: VecDeque::new(),
            call_stack,
        }
    }

    #[inline]
    pub fn run<W: io::Write>(&mut self, out: W) -> Result<(), RuntimeError> {
        let ip = *self.get_ip();

        match self.exec(out) {
            Ok(_) => Ok(()),
            Err(err) => match err {
                // BASIC program exits normally when READ has no more data
                ExecError::NoData => Ok(()),
                _ => Err(RuntimeError {
                    error: err,
                    line_no: self.chunk.line_no(ip),
                }),
            },
        }
    }
    fn exec<W: io::Write>(&mut self, out: W) -> Result<(), ExecError> {
        assert!(self.chunk.len() > 0, "Empty chunk");

        let mut printer = Printer::new_buffered(out);

        loop {
            let byte = self.read_byte()?;
            let instr = OpCode::from_u8(byte).ok_or(ExecError::DecodeError(byte))?;
            match instr {
                OpCode::Stop => return Ok(()),
                OpCode::PrintStart => {
                    printer.write_start();
                }
                OpCode::PrintEnd => {
                    printer.write_end();
                }
                OpCode::PrintExpr => {
                    let value = self.pop_number()?;
                    printer.write_num(value)?;
                }
                OpCode::PrintLabel => {
                    let s: &String = self.read_operand_ref()?;
                    printer.write_str(s)?;
                }
                OpCode::PrintAdvance3 => {
                    printer.advance_to_multiple(3)?;
                }
                OpCode::PrintAdvance15 => {
                    printer.advance_to_multiple(15)?;
                }
                OpCode::InitArray => {
                    let var: Variable = self.read_inline_operand()?;
                    let arr = Array::new(DEFAULT_ARRAY_SIZE);
                    self.global_lists.insert(var, arr);
                }
                OpCode::InitArray2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let arr = Array::new([DEFAULT_ARRAY_SIZE, DEFAULT_ARRAY_SIZE]);
                    self.global_tables.insert(var, arr);
                }
                OpCode::Noop => continue,
                OpCode::Jump => {
                    let jump_point: JumpPoint = self.read_operand()?;
                    *self.get_ip() = jump_point.0;
                }
                OpCode::JumpTrue => {
                    let value = self.pop_value()?;
                    let jump_point: JumpPoint = self.read_operand()?;

                    match value {
                        Variant::True(_) => {
                            *self.get_ip() = jump_point.0;
                        }
                        Variant::False(_) => {}
                        _ => return Err(ExecError::TypeError("not a bool")),
                    }
                }
                OpCode::JumpFalse => {
                    let value = self.pop_value()?;
                    let jump_point: JumpPoint = self.read_operand()?;

                    match value {
                        Variant::True(_) => {}
                        Variant::False(_) => {
                            *self.get_ip() = jump_point.0;
                        }
                        _ => return Err(ExecError::TypeError("not a bool")),
                    }
                }
                OpCode::Constant => {
                    let value: f64 = self.read_operand()?;
                    self.push_value(value);
                }
                OpCode::Pop => {
                    let _ = self.pop_value();
                }
                OpCode::Dup => {
                    if let Some(value) = self.peek(0) {
                        self.push_value(value);
                    }
                }
                OpCode::Swap => {
                    let v1 = self.pop_value()?;
                    let v2 = self.pop_value()?;
                    self.push_value(v1);
                    self.push_value(v2);
                }
                OpCode::CallNative => {
                    let func: Func = self.read_inline_operand()?;
                    let x = self.pop_number()?;
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
                    let v = self.pop_number();
                    // TODO: check callframe to distinguish
                    // func v. subroutine, instead of using Ok
                    // match
                    self.call_stack.pop_back();
                    if let Ok(v) = v {
                        self.push_value(v);
                    }
                }
                OpCode::Call => {
                    let current_depth = self.current_frame().depth;
                    let func: Func = self.read_inline_operand()?;
                    let x = self.pop_number()?;
                    let new_frame = CallFrame {
                        depth: current_depth + 1,
                        func: Some(func),
                        local: Some(x),
                        ip: 0,
                    };
                    self.call_stack.push_back(new_frame);
                }
                OpCode::Subroutine => {
                    let current_depth = self.current_frame().depth;
                    let jp: JumpPoint = self.read_operand()?;
                    let new_frame = CallFrame {
                        depth: current_depth + 1,
                        func: None,
                        local: None,
                        ip: jp.0,
                    };
                    self.call_stack.push_back(new_frame);
                }
                OpCode::GetLocal => {
                    let frame = self.current_frame();
                    let x = frame
                        .local
                        .ok_or(ExecError::TypeError("function argument not found"))?;
                    self.push_value(x);
                }
                OpCode::GetGlobal => {
                    let var: Variable = self.read_inline_operand()?;
                    let v = self.globals.get(&var).map(|v| *v).unwrap_or(0.0);
                    self.push_value(v);
                }
                OpCode::SetGlobal => {
                    let var: Variable = self.read_inline_operand()?;
                    let value = self.pop_number()?;
                    self.globals.insert(var, value);
                }
                OpCode::GetGlobalArray => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x.to_u8().ok_or(ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let list = self
                        .global_lists
                        .get(&var)
                        .ok_or(ExecError::ListNotFound(var))?;
                    let v = list.get(i)?;
                    self.push_value(v);
                }
                OpCode::SetGlobalArray => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x.to_u8().ok_or(ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let v = self.pop_number()?;
                    let list = self
                        .global_lists
                        .get_mut(&var)
                        .ok_or(ExecError::ListNotFound(var))?;
                    list.set(i, v)?;
                }
                OpCode::GetGlobalArray2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x.to_u8().ok_or(ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let j: u8 = match self.pop_number() {
                        Ok(x) => x.to_u8().ok_or(ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let table = self
                        .global_tables
                        .get(&var)
                        .ok_or(ExecError::TableNotFound(var))?;
                    let v = table.get([i, j])?;
                    self.push_value(v);
                }
                OpCode::SetGlobalArray2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x.to_u8().ok_or(ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let j: u8 = match self.pop_number() {
                        Ok(x) => x.to_u8().ok_or(ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let v = self.pop_number()?;
                    let table = self
                        .global_tables
                        .get_mut(&var)
                        .ok_or(ExecError::TableNotFound(var))?;
                    table.set([i, j], v)?;
                }
                OpCode::SetArrayBound => {
                    let var: Variable = self.read_inline_operand()?;
                    let value = self.pop_number()?;
                    let value = value.to_u8().ok_or(ExecError::IndexError(var, value))?;
                    let list = self
                        .global_lists
                        .get_mut(&var)
                        .ok_or(ExecError::ListNotFound(var))?;
                    list.set_bound(value)?;
                }
                OpCode::SetArrayBound2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let n = self.pop_number()?;
                    let n = n.to_u8().ok_or(ExecError::IndexError(var, n))?;
                    let m = self.pop_number()?;
                    let m = m.to_u8().ok_or(ExecError::IndexError(var, m))?;
                    let table = self
                        .global_tables
                        .get_mut(&var)
                        .ok_or(ExecError::TableNotFound(var))?;
                    table.set_bound([m, n])?;
                }
                OpCode::Negate => {
                    let value = self.pop_number()?;
                    let neg_value = -value;
                    self.push_value(neg_value);
                }
                OpCode::Sign => {
                    let value = self.pop_number()?;
                    self.push_value(value.signum() * ((value != 0.0) as u8) as f64);
                }
                OpCode::Not => {
                    let value = self.pop_value()?;
                    let not_value = match value {
                        Variant::False(_) => Value::true_value(),
                        Variant::True(_) => Value::false_value(),
                        _ => return Err(ExecError::TypeError("not a bool")),
                    };
                    self.push_value(not_value);
                }
                OpCode::Add => {
                    let value = self.binary_op(|a, b| Ok(a + b))?;
                    self.push_value(value);
                }
                OpCode::Sub => {
                    let value = self.binary_op(|a, b| Ok(a - b))?;
                    self.push_value(value);
                }
                OpCode::Mul => {
                    let value = self.binary_op(|a, b| Ok(a * b))?;
                    self.push_value(value);
                }
                OpCode::Pow => {
                    let value = self.binary_op(|a, b| Ok(a.powf(b)))?;
                    self.push_value(value);
                }
                OpCode::Div => {
                    let value = self.binary_op(|a, b| Ok(a / b))?;
                    self.push_value(value);
                }
                OpCode::Equal => {
                    let value = self.binary_op(|a, b| {
                        Ok(if a == b {
                            Value::true_value()
                        } else {
                            Value::false_value()
                        })
                    })?;
                    self.push_value(value);
                }
                OpCode::Less => {
                    let value = self.binary_op(|a, b| {
                        Ok(if a < b {
                            Value::true_value()
                        } else {
                            Value::false_value()
                        })
                    })?;
                    self.push_value(value);
                }
                OpCode::Greater => {
                    let value = self.binary_op(|a, b| {
                        Ok(if a > b {
                            Value::true_value()
                        } else {
                            Value::false_value()
                        })
                    })?;
                    self.push_value(value);
                }
            }
        }
    }

    #[inline]
    fn current_frame(&self) -> &CallFrame {
        self.call_stack.back().unwrap()
    }

    #[inline]
    fn current_chunk(&mut self) -> Result<&mut Chunk, ExecError> {
        let frame = self.call_stack.back_mut().unwrap();
        match frame.func {
            Some(func) => self
                .chunk
                .get_function(&func)
                .ok_or(ExecError::FunctionNotFound(func)),
            _ => Ok(&mut self.chunk),
        }
    }

    #[inline]
    fn get_ip(&mut self) -> &mut usize {
        let frame = self.call_stack.back_mut().unwrap();
        &mut frame.ip
    }

    fn read_byte(&mut self) -> Result<u8, ExecError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let byte = chunk.read_byte(ip);

        *self.get_ip() += 1;

        Ok(byte)
    }
    fn read_operand<T: Operand>(&mut self) -> Result<T, ExecError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let o = chunk.read_operand(ip);
        *self.get_ip() += 2;

        Ok(o)
    }
    fn read_operand_ref<T: Operand>(&mut self) -> Result<&T, ExecError> {
        let ip = *self.get_ip();
        *self.get_ip() += 2;
        let chunk = self.current_chunk()?;
        let o = chunk.read_operand_ref(ip);

        Ok(o)
    }
    fn read_inline_operand<T: InlineOperand>(&mut self) -> Result<T, ExecError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let o = chunk.read_inline_operand(ip);
        *self.get_ip() += 2;

        Ok(o)
    }

    fn push_value<V: Into<Value>>(&mut self, v: V) {
        self.stack.push_back(v.into());
    }

    fn pop_number(&mut self) -> Result<Number, ExecError> {
        let v = self.pop_value()?;
        match v {
            Variant::Number(v) => Ok(v),
            Variant::NoData(_) => Err(ExecError::NoData),
            _ => Err(ExecError::TypeError("not a number")),
        }
    }

    #[inline(always)]
    fn pop_value(&mut self) -> Result<Variant, ExecError> {
        self.stack
            .pop_back()
            .map(Variant::from)
            .ok_or(ExecError::EmptyStack)
    }

    fn peek(&self, distance: usize) -> Option<Value> {
        let n = self.stack.len();
        let index = n - 1 - distance;

        self.stack.get(index).cloned()
    }

    fn binary_op<T, F>(&mut self, f: F) -> Result<T, ExecError>
    where
        F: Fn(Number, Number) -> Result<T, ExecError>,
    {
        let a = self.pop_number()?;
        let b = self.pop_number()?;

        f(b, a)
    }
}
