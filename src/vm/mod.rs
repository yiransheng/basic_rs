use std::collections::VecDeque;
use std::error;
use std::f64;
use std::fmt;
use std::io;
use std::mem;

use num_traits::{FromPrimitive, ToPrimitive};
use rand::Rng;
use rustc_hash::FxHashMap;

use crate::ast::function::Func;
use crate::ast::*;

pub mod value;

mod array;
mod chunk;
mod data;
mod line_mapping;
mod opcode;
mod print;

pub use self::chunk::*;
pub use self::opcode::*;
pub use self::value::{FuncId, FuncIdGen};

use self::array::{Array, Error as ArrayError};
use self::print::{PrintError, Printer};
use self::value::*;

pub const DEFAULT_ARRAY_SIZE: u8 = 11;

#[derive(Debug)]
pub struct CallFrame {
    depth: usize,
    ip: usize,
    context: Option<FuncId>,
    locals: FxHashMap<Variable, Number>,
}

pub struct VM {
    chunk: Chunk,
    user_fns: FxHashMap<FuncId, Chunk>,

    globals: FxHashMap<Variable, Number>,
    functions: FxHashMap<Func, FuncId>,
    global_lists: FxHashMap<Variable, Array<u8>>,
    global_tables: FxHashMap<Variable, Array<[u8; 2]>>,

    stack: VecDeque<Value>,
    call_stack: VecDeque<CallFrame>,
}

#[derive(Debug)]
pub struct RuntimeError {
    pub error: ExecError,
    pub line_no: usize,
}

#[derive(Debug)]
pub enum ExecError {
    NoData,
    EmptyStack,
    ListNotFound(Variable),
    TableNotFound(Variable),
    IndexError(Variable, f64),
    TypeError(&'static str),
    ValueError(&'static str),
    ArrayError(ArrayError),
    PrintError(PrintError),
    DecodeError(u8),
    FunctionNotFound,
}

impl fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::error::Error;

        let desc = self.description();
        match self {
            ExecError::ListNotFound(var) => write!(f, "{}: {}", desc, var),
            ExecError::TableNotFound(var) => write!(f, "{}: {}", desc, var),
            ExecError::IndexError(var, v) => {
                write!(f, "{}, variable: {}, index: {}", desc, var, v)
            }
            ExecError::TypeError(s) => write!(f, "TypeError: {}", s),
            ExecError::ValueError(s) => write!(f, "ValueError: {}", s),
            ExecError::ArrayError(err) => err.fmt(f),
            ExecError::PrintError(err) => err.fmt(f),
            ExecError::DecodeError(b) => {
                write!(f, "Failed to decode instruction: {}", b)
            }
            ExecError::EmptyStack
            | ExecError::NoData
            | ExecError::FunctionNotFound => write!(f, "{}", desc),
        }
    }
}

impl error::Error for ExecError {
    fn description(&self) -> &str {
        match self {
            ExecError::NoData => "No data",
            ExecError::EmptyStack => "Empty stack",
            ExecError::ListNotFound(_) => "Use uninitialized list",
            ExecError::TableNotFound(_) => "Use uninitialized table",
            ExecError::IndexError(..) => "Index error",
            ExecError::TypeError(_) => "Type error",
            ExecError::ValueError(_) => "Value error",
            ExecError::ArrayError(err) => err.description(),
            ExecError::PrintError(err) => err.description(),
            ExecError::DecodeError(_) => "Decode error",
            ExecError::FunctionNotFound => "Function not found",
        }
    }
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
    pub fn new(main_id: FuncId, mut chunks: FxHashMap<FuncId, Chunk>) -> Self {
        let chunk = chunks.remove(&main_id).unwrap();

        let mut call_stack = VecDeque::with_capacity(16);
        let call_frame = CallFrame {
            depth: 0,
            ip: 0,
            context: None,
            locals: FxHashMap::default(),
        };
        call_stack.push_back(call_frame);
        VM {
            chunk,
            user_fns: chunks,

            globals: FxHashMap::default(),
            functions: FxHashMap::default(),
            global_lists: FxHashMap::default(),
            global_tables: FxHashMap::default(),
            stack: VecDeque::with_capacity(256),

            call_stack,
        }
    }

    pub fn disassemble<W: io::Write>(&mut self, mut out: W) {
        use self::disassembler::Disassembler;

        let mut main_chunk = Disassembler::new(&mut self.chunk, &mut out);
        main_chunk.disassemble();

        for (func_id, chunk) in self.user_fns.iter_mut() {
            let _ = writeln!(&mut out, "Chunk: {}\n", func_id);
            let mut fn_chunk = Disassembler::new(chunk, &mut out);
            fn_chunk.disassemble();
        }
    }

    pub fn reset(&mut self) {
        self.chunk.reset();
        for c in self.user_fns.values_mut() {
            c.reset();
        }
        self.globals.clear();
        self.global_lists.clear();
        self.global_tables.clear();
        self.functions.clear();
        self.stack.clear();
        self.call_stack.clear();

        let call_frame = CallFrame {
            depth: 0,
            ip: 0,
            context: None,
            locals: FxHashMap::default(),
        };

        self.call_stack.push_back(call_frame);
    }

    #[inline]
    pub fn run<W: io::Write, R: Rng>(
        &mut self,
        out: W,
        rng: &mut R,
    ) -> Result<(), RuntimeError> {
        match self.exec(out, rng) {
            Ok(_) => Ok(()),
            Err(err) => {
                let ip = *self.get_ip() - 1;
                match err {
                    // BASIC program exits normally when READ has no more data
                    ExecError::NoData => Ok(()),
                    _ => Err(RuntimeError {
                        error: err,
                        line_no: self.chunk.line_no(ip),
                    }),
                }
            }
        }
    }
    fn exec<W: io::Write, R: Rng>(
        &mut self,
        out: W,
        rng: &mut R,
    ) -> Result<(), ExecError> {
        assert!(self.chunk.len() > 0, "Empty chunk");

        let mut printer = Printer::new_buffered(out);

        loop {
            let byte = self.read_byte()?;
            let instr = OpCode::from_u8(byte)
                .ok_or_else(|| ExecError::DecodeError(byte))?;
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
                    let arr =
                        Array::new([DEFAULT_ARRAY_SIZE, DEFAULT_ARRAY_SIZE]);
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
                OpCode::FnConstant => {
                    let func_id: FuncId = self.read_inline_operand()?;
                    self.push_value(func_id);
                }
                OpCode::Pop => {
                    let _ = self.pop_value();
                }
                OpCode::Dup => {
                    if let Some(value) = self.peek(0) {
                        self.push_value(value);
                    }
                }
                OpCode::CallNative => {
                    let func: Func = self.read_inline_operand()?;
                    // BASIC RND function does not actually take
                    // arguments, `RND(X)` is only a syntactic requirement
                    let x = if func != Func::Rnd {
                        self.pop_number()?
                    } else {
                        0.0
                    };
                    let y = match func {
                        Func::Sin => x.sin(),
                        Func::Cos => x.cos(),
                        Func::Tan => x.tan(),
                        Func::Atn => x.atan(),
                        Func::Exp => x.exp(),
                        Func::Abs => x.abs(),
                        Func::Log => x.ln(),
                        Func::Sqr => x.sqrt(),
                        Func::Rnd => rng.gen(),
                        Func::Int => x.trunc(),
                        _ => unreachable!("Compiler bug"),
                    };
                    self.push_value(y);
                }
                OpCode::Return => match self.current_frame().context {
                    Some(_) => {
                        // function call
                        let v = self.pop_number()?;
                        self.call_stack.pop_back();
                        self.push_value(v);
                    }
                    _ => {
                        // subroutine call
                        let ret_frame = self.call_stack.pop_back().unwrap();
                        let current_frame = self.current_frame_mut();
                        current_frame.locals = ret_frame.locals;
                    }
                },
                OpCode::Call => {
                    let current_depth = self.current_frame().depth;
                    let func = match self.pop_value()? {
                        Variant::Function(func_id) => func_id,
                        _ => return Err(ExecError::TypeError("not a function")),
                    };
                    let new_frame = CallFrame {
                        depth: current_depth + 1,
                        context: Some(func),
                        locals: FxHashMap::default(),
                        ip: 0,
                    };
                    self.call_stack.push_back(new_frame);
                }
                OpCode::Subroutine => {
                    let jp: JumpPoint = self.read_operand()?;
                    let current_frame = self.current_frame_mut();

                    // transfer locals to subroutine callframe
                    let locals = mem::replace(
                        &mut current_frame.locals,
                        FxHashMap::default(),
                    );

                    let new_frame = CallFrame {
                        depth: current_frame.depth,
                        context: None,
                        locals,
                        ip: jp.0,
                    };
                    self.call_stack.push_back(new_frame);
                }

                OpCode::GetLocal => {
                    let var: Variable = self.read_inline_operand()?;
                    let frame = self.current_frame();

                    let x =
                        frame.locals.get(&var).cloned().ok_or_else(|| {
                            ExecError::ValueError("Value not found")
                        })?;

                    self.push_value(x);
                }
                OpCode::SetLocal => {
                    let x = self.pop_number()?;
                    let var: Variable = self.read_inline_operand()?;
                    let frame = self.current_frame_mut();

                    frame.locals.insert(var, x);
                }
                OpCode::GetFunc => {
                    let fname: Func = self.read_inline_operand()?;
                    // TODO: error handling
                    let func_id = self.functions.get(&fname).cloned().unwrap();
                    self.push_value(func_id);
                }
                OpCode::SetFunc => {
                    let fname: Func = self.read_inline_operand()?;
                    // TODO: error handling
                    let value = self.pop_value()?;
                    match value {
                        Variant::Function(func_id) => {
                            self.functions.insert(fname, func_id);
                        }
                        _ => return Err(ExecError::TypeError("not a function")),
                    }
                }
                OpCode::ReadGlobal => {
                    let var: Variable = self.read_inline_operand()?;
                    let value = self.read_number()?;
                    self.globals.insert(var, value);
                }
                OpCode::ReadGlobalArray => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let v = self.read_number()?;
                    let list = self
                        .global_lists
                        .get_mut(&var)
                        .ok_or_else(|| ExecError::ListNotFound(var))?;
                    list.set(i, v)?;
                }
                OpCode::ReadGlobalArray2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let j: u8 = match self.pop_number() {
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let v = self.read_number()?;
                    let table = self
                        .global_tables
                        .get_mut(&var)
                        .ok_or_else(|| ExecError::TableNotFound(var))?;
                    table.set([i, j], v)?;
                }
                OpCode::GetGlobal => {
                    let var: Variable = self.read_inline_operand()?;
                    let v = self.globals.get(&var).cloned().unwrap_or(0.0);
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
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let list = self
                        .global_lists
                        .get(&var)
                        .ok_or_else(|| ExecError::ListNotFound(var))?;
                    let v = list.get(i)?;
                    self.push_value(v);
                }
                OpCode::SetGlobalArray => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let v = self.pop_number()?;
                    let list = self
                        .global_lists
                        .get_mut(&var)
                        .ok_or_else(|| ExecError::ListNotFound(var))?;
                    list.set(i, v)?;
                }
                OpCode::GetGlobalArray2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let j: u8 = match self.pop_number() {
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let table = self
                        .global_tables
                        .get(&var)
                        .ok_or_else(|| ExecError::TableNotFound(var))?;
                    let v = table.get([i, j])?;
                    self.push_value(v);
                }
                OpCode::SetGlobalArray2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: u8 = match self.pop_number() {
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let j: u8 = match self.pop_number() {
                        Ok(x) => x
                            .to_u8()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let v = self.pop_number()?;
                    let table = self
                        .global_tables
                        .get_mut(&var)
                        .ok_or_else(|| ExecError::TableNotFound(var))?;
                    table.set([i, j], v)?;
                }
                OpCode::SetArrayBound => {
                    let var: Variable = self.read_inline_operand()?;
                    let value = self.pop_number()?;
                    let value = value
                        .to_u8()
                        .ok_or_else(|| ExecError::IndexError(var, value))?;
                    let list = self
                        .global_lists
                        .get_mut(&var)
                        .ok_or_else(|| ExecError::ListNotFound(var))?;
                    list.set_bound(value)?;
                }
                OpCode::SetArrayBound2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let n = self.pop_number()?;
                    let n = n
                        .to_u8()
                        .ok_or_else(|| ExecError::IndexError(var, n))?;
                    let m = self.pop_number()?;
                    let m = m
                        .to_u8()
                        .ok_or_else(|| ExecError::IndexError(var, m))?;
                    let table = self
                        .global_tables
                        .get_mut(&var)
                        .ok_or_else(|| ExecError::TableNotFound(var))?;
                    table.set_bound([m, n])?;
                }
                OpCode::Negate => {
                    let value = self.pop_number()?;
                    let neg_value = -value;
                    self.push_value(neg_value);
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
                        Ok(if (a - b).abs() < f64::EPSILON {
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
                OpCode::LoopTest => {
                    let current = self.pop_number()?;
                    let step = self.peek_number(0)?;
                    let target = self.peek_number(1)?;

                    if (step > 0.0 && current > target)
                        || (step < 0.0 && current < target)
                    {
                        let _ = self.pop_value();
                        let _ = self.pop_value();
                        self.push_value(Value::true_value());
                    } else {
                        self.push_value(Value::false_value());
                    }
                }
            }
        }
    }

    #[inline]
    fn current_frame(&self) -> &CallFrame {
        self.call_stack.back().unwrap()
    }

    #[inline]
    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.back_mut().unwrap()
    }

    #[inline]
    fn current_chunk(&mut self) -> Result<&mut Chunk, ExecError> {
        let frame = self.call_stack.back().unwrap();

        match frame.context {
            Some(func_id) => self
                .user_fns
                .get_mut(&func_id)
                .ok_or_else(|| ExecError::FunctionNotFound),
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
    fn read_inline_operand<T: InlineOperand>(
        &mut self,
    ) -> Result<T, ExecError> {
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
        match self.pop_value() {
            Ok(Variant::Number(v)) => Ok(v),
            Ok(_) => Err(ExecError::TypeError("not a number")),
            Err(e) => Err(e),
        }
    }

    fn read_number(&mut self) -> Result<Number, ExecError> {
        let chunk = self.current_chunk()?;
        let n = chunk.pop_data().ok_or_else(|| ExecError::NoData)?;

        Ok(n)
    }

    fn peek_number(&self, distance: usize) -> Result<Number, ExecError> {
        match self.peek(distance).map(Variant::from) {
            Some(Variant::Number(n)) => Ok(n),
            Some(_) => Err(ExecError::TypeError("not a number")),
            None => Err(ExecError::EmptyStack),
        }
    }

    #[inline(always)]
    fn pop_value(&mut self) -> Result<Variant, ExecError> {
        self.stack
            .pop_back()
            .map(Variant::from)
            .ok_or_else(|| ExecError::EmptyStack)
    }

    #[inline(always)]
    fn peek(&self, distance: usize) -> Option<Value> {
        let n = self.stack.len();
        let index = n.saturating_sub(distance + 1);

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
