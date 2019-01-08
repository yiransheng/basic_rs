use std::collections::VecDeque;
use std::error;
use std::f64;
use std::fmt;
use std::io;

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
pub use self::data::DataStack;
pub use self::opcode::*;
pub use self::value::{FuncId, FuncIdGen};

use self::array::{Array, Error as ArrayError};
use self::print::{PrintError, Printer};
use self::value::*;

pub const DEFAULT_ARRAY_SIZE: usize = 11;

#[derive(Debug)]
pub struct CallFrame {
    depth: usize,
    ip: usize,
    sp: usize,
    func: Option<FuncId>,
}

pub struct VM {
    chunk: Chunk,
    fn_chunks: FxHashMap<FuncId, Chunk>,
    data: DataStack<f64>,

    globals: FxHashMap<Variable, Number>,
    functions: FxHashMap<Func, FuncId>,
    global_lists: FxHashMap<Variable, Array<usize>>,
    global_tables: FxHashMap<Variable, Array<[usize; 2]>>,

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
    InputError,
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
            ExecError::InputError => write!(f, "{}", desc),
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
            ExecError::InputError => "Input error",
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
    pub fn new(
        main_id: FuncId,
        mut chunks: FxHashMap<FuncId, Chunk>,
        data: Vec<f64>,
    ) -> Self {
        let chunk = chunks.remove(&main_id).unwrap();

        let mut call_stack = VecDeque::with_capacity(16);
        let call_frame = CallFrame {
            depth: 0,
            ip: 0,
            sp: 0,
            func: None,
        };
        call_stack.push_back(call_frame);
        VM {
            chunk,
            fn_chunks: chunks,
            data: DataStack::new(data),

            globals: FxHashMap::default(),
            functions: FxHashMap::default(),
            global_lists: FxHashMap::default(),
            global_tables: FxHashMap::default(),
            stack: VecDeque::with_capacity(256),

            call_stack,
        }
    }

    pub fn disassemble<W: io::Write>(&self, mut out: W) {
        use self::disassembler::Disassembler;

        let mut main_chunk = Disassembler::new(&self.chunk, &mut out);
        main_chunk.disassemble();

        for (func_id, chunk) in self.fn_chunks.iter() {
            let _ = writeln!(&mut out, "Chunk: {}\n", func_id);
            let mut fn_chunk = Disassembler::new(chunk, &mut out);
            fn_chunk.disassemble();
        }
    }

    pub fn reset(&mut self) {
        self.globals.clear();
        self.global_lists.clear();
        self.global_tables.clear();
        self.functions.clear();
        self.stack.clear();
        self.call_stack.clear();

        let call_frame = CallFrame {
            depth: 0,
            ip: 0,
            sp: 0,
            func: None,
        };

        self.call_stack.push_back(call_frame);
    }

    pub fn run_with_input<W: io::Write, R: Rng>(
        &mut self,
        inp: io::StdinLock,
        out: W,
        rng: &mut R,
    ) -> Result<(), RuntimeError> {
        match self.exec(inp, out, rng) {
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

    #[cfg(test)]
    pub fn run<W: io::Write, R: Rng>(
        &mut self,
        out: W,
        rng: &mut R,
    ) -> Result<(), RuntimeError> {
        match self.exec(io::BufReader::new(io::empty()), out, rng) {
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

    fn exec<I: io::BufRead, W: io::Write, R: Rng>(
        &mut self,
        mut inp: I,
        out: W,
        rng: &mut R,
    ) -> Result<(), ExecError> {
        assert!(self.chunk.len() > 0, "Empty chunk");

        let mut printer = Printer::new_buffered(out);
        let mut input_string = String::new();

        loop {
            let byte = self.read_byte()?;
            let instr = OpCode::from_u8(byte)
                .ok_or_else(|| ExecError::DecodeError(byte))?;
            match instr {
                OpCode::Noop => continue,
                OpCode::Stop => return Ok(()),
                OpCode::PrintNewline => {
                    printer.writeln()?;
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
                OpCode::InitArray1d => {
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
                OpCode::DefineDim1d => {
                    let var: Variable = self.read_inline_operand()?;
                    let n: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let list = self
                        .global_lists
                        .get_mut(&var)
                        .ok_or_else(|| ExecError::ListNotFound(var))?;
                    list.set_bound(n + 1)?;
                }
                OpCode::DefineDim2d => {
                    let var: Variable = self.read_inline_operand()?;
                    let m: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let n: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let table = self
                        .global_tables
                        .get_mut(&var)
                        .ok_or_else(|| ExecError::TableNotFound(var))?;
                    table.set_bound([m + 1, n + 1])?;
                }
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
                OpCode::Read => {
                    let value = self.read_number()?;
                    self.push_value(value);
                }
                OpCode::Input => {
                    printer.flush()?;
                    input_string.clear();

                    inp.read_line(&mut input_string)
                        .map_err(|_| ExecError::InputError)?;

                    let s = &input_string.trim();
                    let v: f64 = if s.is_empty() {
                        0.0
                    } else {
                        s.parse().map_err(|_| ExecError::InputError)?
                    };

                    self.push_value(v);
                }
                OpCode::Rand => {
                    let v: f64 = rng.gen();
                    self.push_value(v);
                }
                OpCode::BindFunc => {
                    let func: Func = self.read_inline_operand()?;
                    let func_id: FuncId = self.read_inline_operand()?;
                    self.functions.insert(func, func_id);
                }
                OpCode::GetFunc => {
                    let fname: Func = self.read_inline_operand()?;
                    // TODO: error handling
                    let func_id = self.functions.get(&fname).cloned().unwrap();
                    self.push_value(func_id);
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
                        Func::Int => x.trunc(),
                        _ => unreachable!("Compiler bug"),
                    };
                    self.push_value(y);
                }
                OpCode::Return => {
                    let frame = self.call_stack.pop_back().unwrap();
                    if frame.func.is_none() {
                        return Ok(());
                    }
                    while self.stack.len() > frame.sp {
                        self.stack.pop_back();
                    }
                }
                OpCode::ReturnValue => {
                    let ret = self.pop_number()?;
                    let frame = self.call_stack.pop_back().unwrap();
                    while self.stack.len() > frame.sp {
                        self.stack.pop_back();
                    }
                    self.push_value(ret);
                }
                OpCode::Call => {
                    let current_depth = self.current_frame().depth;
                    let func_id: FuncId = self.read_inline_operand()?;
                    let n_args = (self.read_byte()?) as usize;

                    let n_values = self.stack.len();
                    if n_args > n_values {
                        return Err(ExecError::TypeError(
                            "not enough arguments",
                        ));
                    }

                    let new_frame = CallFrame {
                        depth: current_depth + 1,
                        ip: 0,
                        sp: n_values - n_args,
                        func: Some(func_id),
                    };
                    self.call_stack.push_back(new_frame);
                }
                OpCode::CallIndirect => {
                    let n_args = (self.read_byte()?) as usize;
                    let current_depth = self.current_frame().depth;
                    let func_id: FuncId = match self.pop_value()? {
                        Variant::Function(func_id) => func_id,
                        _ => return Err(ExecError::TypeError("not callable")),
                    };

                    let n_values = self.stack.len();
                    if n_args > n_values {
                        return Err(ExecError::TypeError(
                            "not enough arguments",
                        ));
                    }

                    let new_frame = CallFrame {
                        depth: current_depth + 1,
                        ip: 0,
                        sp: n_values - n_args,
                        func: Some(func_id),
                    };
                    self.call_stack.push_back(new_frame);
                }
                OpCode::DeclLocal => {
                    let n = self.read_byte()?;
                    for _ in 0..n {
                        self.push_value(Undefined);
                    }
                }
                OpCode::GetLocal => {
                    let var: LocalVar = self.read_inline_operand()?;
                    let v: Variant = self.get_local(var)?;

                    match v {
                        Variant::Number(x) => {
                            self.push_value(x);
                        }
                        _ => return Err(ExecError::TypeError("not a number")),
                    }
                }
                OpCode::SetLocal => {
                    let x = self.pop_number()?;
                    let var: LocalVar = self.read_inline_operand()?;
                    self.set_local(var, Variant::Number(x))?;
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
                OpCode::GetGlobalArray1d => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
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
                OpCode::SetGlobalArray1d => {
                    let var: Variable = self.read_inline_operand()?;
                    let i: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
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
                    let i: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let j: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
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
                    let i: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
                            .ok_or_else(|| ExecError::IndexError(var, x))?,
                        Err(e) => return Err(e),
                    };
                    let j: usize = match self.pop_number() {
                        Ok(x) => x
                            .to_usize()
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
                OpCode::Rem => {
                    let value = self.binary_op(|a, b| Ok(a % b))?;
                    self.push_value(value);
                }
                OpCode::CopySign => {
                    let value = self.binary_op(|a, b| Ok(a * b.signum()))?;
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
            }
        }
    }

    #[inline]
    fn current_frame(&self) -> &CallFrame {
        self.call_stack.back().unwrap()
    }

    #[inline(always)]
    fn current_chunk(&mut self) -> Result<&mut Chunk, ExecError> {
        let frame = self.call_stack.back().unwrap();

        match frame.func {
            Some(func_id) => self
                .fn_chunks
                .get_mut(&func_id)
                .ok_or_else(|| ExecError::FunctionNotFound),
            _ => Ok(&mut self.chunk),
        }
    }

    #[inline(always)]
    fn get_ip(&mut self) -> &mut usize {
        let frame = self.call_stack.back_mut().unwrap();
        &mut frame.ip
    }

    #[inline(always)]
    fn read_byte(&mut self) -> Result<u8, ExecError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let byte = chunk.read_byte(ip);

        *self.get_ip() += 1;

        Ok(byte)
    }

    #[inline(always)]
    fn read_operand<T: Operand>(&mut self) -> Result<T, ExecError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let o = chunk.read_operand(ip);
        *self.get_ip() += 2;

        Ok(o)
    }

    #[inline(always)]
    fn read_operand_ref<T: Operand>(&mut self) -> Result<&T, ExecError> {
        let ip = *self.get_ip();
        *self.get_ip() += 2;
        let chunk = self.current_chunk()?;
        let o = chunk.read_operand_ref(ip);

        Ok(o)
    }

    #[inline(always)]
    fn read_inline_operand<T: InlineOperand>(
        &mut self,
    ) -> Result<T, ExecError> {
        let ip = *self.get_ip();
        let chunk = self.current_chunk()?;
        let o = chunk.read_inline_operand(ip);
        *self.get_ip() += 2;

        Ok(o)
    }

    fn get_local<V: From<Value>>(
        &self,
        index: LocalVar,
    ) -> Result<V, ExecError> {
        let index: usize = index.into();
        let index = self.current_frame().sp + index;
        if let Some(v) = self.stack.get(index) {
            Ok(V::from(*v))
        } else {
            Err(ExecError::EmptyStack)
        }
    }

    fn set_local<V: Into<Value>>(
        &mut self,
        index: LocalVar,
        v: V,
    ) -> Result<(), ExecError> {
        let index: usize = index.into();
        let index = self.current_frame().sp + index;
        if let Some(vref) = self.stack.get_mut(index) {
            *vref = v.into();
            Ok(())
        } else {
            Err(ExecError::EmptyStack)
        }
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
        let n = self.data.pop_back().ok_or_else(|| ExecError::NoData)?;

        Ok(n)
    }

    #[inline(always)]
    fn pop_value(&mut self) -> Result<Variant, ExecError> {
        self.stack
            .pop_back()
            .map(Variant::from)
            .ok_or_else(|| ExecError::EmptyStack)
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
