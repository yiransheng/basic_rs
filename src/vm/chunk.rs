use std::collections::VecDeque;

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num_traits::{FromPrimitive, ToPrimitive};

use super::data::DataStack;
use super::line_mapping::LineMapping;
use super::opcode::OpCode;
use super::value::FuncId;
use crate::ast::function::Func;
use crate::ast::variable::Variable;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct JumpPoint(pub usize);

pub trait InlineOperand: Into<[u8; 2]> {
    fn from_bytes_unchecked(bytes: [u8; 2]) -> Self;
}

impl InlineOperand for Variable {
    #[inline(always)]
    fn from_bytes_unchecked(bytes: [u8; 2]) -> Self {
        Variable::from_bytes_unchecked(bytes)
    }
}

impl Into<[u8; 2]> for Func {
    fn into(self) -> [u8; 2] {
        let b = self.to_u8().unwrap();
        [b, 0]
    }
}

impl InlineOperand for Func {
    #[inline(always)]
    fn from_bytes_unchecked(bytes: [u8; 2]) -> Self {
        Func::from_u8(bytes[0]).unwrap()
    }
}

impl Into<[u8; 2]> for FuncId {
    fn into(self) -> [u8; 2] {
        let b = self.raw();
        [b, 0]
    }
}

impl InlineOperand for FuncId {
    #[inline(always)]
    fn from_bytes_unchecked(bytes: [u8; 2]) -> Self {
        FuncId::from_u8(bytes[0]).unwrap()
    }
}

pub trait Operand: Clone {
    fn storage(chunk: &mut Chunk) -> &mut Vec<Self>
    where
        Self: Sized;

    fn add_to_chunk(self, chunk: &mut Chunk) -> u16
    where
        Self: Sized,
    {
        let where_to = Self::storage(chunk);
        let index = where_to.len();
        assert!(index < (u16::max_value() as usize));

        where_to.push(self);

        let index = index as u16;

        chunk.write_index(index);

        index
    }

    fn read_from_chunk(offset: usize, chunk: &mut Chunk) -> Self
    where
        Self: Sized,
    {
        let index = chunk.read_index(offset) as usize;
        let where_to = Self::storage(chunk);

        where_to[index].clone()
    }

    fn read_ref_from_chunk(offset: usize, chunk: &mut Chunk) -> &Self
    where
        Self: Sized,
    {
        let index = chunk.read_index(offset) as usize;
        let where_to = Self::storage(chunk);

        &where_to[index]
    }
}

impl Operand for JumpPoint {
    fn storage(chunk: &mut Chunk) -> &mut Vec<Self> {
        &mut chunk.jump_points
    }
}

impl Operand for f64 {
    fn storage(chunk: &mut Chunk) -> &mut Vec<Self> {
        &mut chunk.constants
    }
}

impl Operand for String {
    fn storage(chunk: &mut Chunk) -> &mut Vec<Self> {
        &mut chunk.strings
    }
}

pub struct Chunk {
    code: Vec<u8>,

    data: DataStack<f64>,
    constants: Vec<f64>,

    jump_points: Vec<JumpPoint>,
    strings: Vec<String>,
    line_map: LineMapping,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),

            data: DataStack::new(),
            constants: Vec::new(),

            jump_points: Vec::new(),
            strings: Vec::new(),
            line_map: LineMapping::new(),
        }
    }

    pub fn reset(&mut self) {
        self.data.reset();
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    #[inline(always)]
    pub fn write_opcode(&mut self, code: OpCode, line: usize) {
        self.write(code as u8, line)
    }

    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte.into());
        self.line_map.add_mapping(line, self.len());
    }

    pub fn line_no(&self, offset: usize) -> usize {
        self.line_map.find_line(offset)
    }

    pub fn add_operand<O: Operand>(&mut self, o: O, line: usize) -> u16 {
        let slot = o.add_to_chunk(self);
        self.line_map.add_mapping(line, self.len());
        slot
    }

    pub fn set_operand<O: Operand>(&mut self, index: u16, o: O) {
        let storage = O::storage(self);
        storage[index as usize] = o;
    }

    pub fn add_inline_operand<O: InlineOperand>(&mut self, o: O, line: usize) {
        let bytes = o.into();
        self.write(bytes[0], line);
        self.write(bytes[1], line);
        self.line_map.add_mapping(line, self.len());
    }

    #[inline(always)]
    pub fn read_byte(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    #[inline(always)]
    pub fn read_operand<O: Operand>(&mut self, offset: usize) -> O {
        O::read_from_chunk(offset, self)
    }

    #[inline(always)]
    pub fn read_operand_ref<O: Operand>(&mut self, offset: usize) -> &O {
        O::read_ref_from_chunk(offset, self)
    }

    #[inline(always)]
    pub fn read_inline_operand<O: InlineOperand>(
        &mut self,
        offset: usize,
    ) -> O {
        let bytes = [self.read_byte(offset), self.read_byte(offset + 1)];
        O::from_bytes_unchecked(bytes)
    }

    #[inline(always)]
    pub fn pop_data(&mut self) -> Option<f64> {
        self.data.pop_front()
    }

    fn write_index(&mut self, index: u16) {
        self.code.write_u16::<LittleEndian>(index).unwrap();
    }

    fn read_index(&self, offset: usize) -> u16 {
        (&self.code[offset..]).read_u16::<LittleEndian>().unwrap()
    }
}

pub mod from_ir {
    use rustc_hash::FxHashMap;

    use super::*;
    use crate::ir::*;

    //TODO: add a &'static str field for easy debugging
    #[derive(Debug, Copy, Clone)]
    pub struct WriteError;

    pub struct ChunkWriter {
        jp_label_map: FxHashMap<Label, JumpPoint>,
        jp_indices: Vec<(u16, Label)>,
        chunk: Chunk,
    }
    impl Default for ChunkWriter {
        fn default() -> Self {
            ChunkWriter {
                jp_label_map: FxHashMap::default(),
                jp_indices: Vec::new(),
                chunk: Chunk::new(),
            }
        }
    }

    impl Visitor for ChunkWriter {
        type Output = Chunk;
        type Error = WriteError;

        fn finish(mut self) -> Result<Self::Output, WriteError> {
            for (index, label) in self.jp_indices.iter() {
                let jp = self.jp_label_map.get(label).ok_or(WriteError)?;
                self.chunk.set_operand(*index, *jp);
            }
            Ok(self.chunk)
        }

        fn visit_instruction(
            &mut self,
            instr: Instruction,
        ) -> Result<(), WriteError> {
            use self::InstructionKind::*;

            let Instruction {
                kind,
                line_no,
                label,
            } = instr;

            if let Some(label) = label {
                let jp = JumpPoint(self.chunk.len());
                if let Some(_) = self.jp_label_map.insert(label, jp) {
                    return Err(WriteError);
                }
            }

            match kind {
                Data(n) => self.chunk.data.push_back(n),
                Constant(n) => {
                    self.chunk.write_opcode(OpCode::Constant, line_no);
                    self.chunk.add_operand(n, line_no);
                }
                Return => {
                    self.chunk.write_opcode(OpCode::Return, line_no);
                }
                Stop => {
                    self.chunk.write_opcode(OpCode::Stop, line_no);
                }
                Jump(label) => {
                    self.chunk.write_opcode(OpCode::Jump, line_no);
                    let jp_index =
                        self.chunk.add_operand(JumpPoint(0), line_no);
                    self.jp_indices.push((jp_index, label));
                }
                JumpTrue(label) => {
                    self.chunk.write_opcode(OpCode::JumpTrue, line_no);
                    let jp_index =
                        self.chunk.add_operand(JumpPoint(0), line_no);
                    self.jp_indices.push((jp_index, label));
                }
                JumpFalse(label) => {
                    self.chunk.write_opcode(OpCode::JumpFalse, line_no);
                    let jp_index =
                        self.chunk.add_operand(JumpPoint(0), line_no);
                    self.jp_indices.push((jp_index, label));
                }
                Subroutine(label) => {
                    self.chunk.write_opcode(OpCode::Subroutine, line_no);
                    let jp_index =
                        self.chunk.add_operand(JumpPoint(0), line_no);
                    self.jp_indices.push((jp_index, label));
                }
                CallNative(func) => {
                    self.chunk.write_opcode(OpCode::CallNative, line_no);
                    self.chunk.add_inline_operand(func, line_no);
                }
                MapFunc(func, func_id) => {
                    self.chunk.write_opcode(OpCode::FnConstant, line_no);
                    self.chunk.add_inline_operand(func_id, line_no);
                    self.chunk.write_opcode(OpCode::SetFunc, line_no);
                    self.chunk.add_inline_operand(func, line_no);
                }
                Call(func) => {
                    self.chunk.write_opcode(OpCode::GetFunc, line_no);
                    self.chunk.add_inline_operand(func, line_no);
                    self.chunk.write_opcode(OpCode::Call, line_no);
                }
                DefineGlobal(_) => {}
                DefineLocal(_) => {}
                SetLocal(var) => {
                    self.chunk.write_opcode(OpCode::SetLocal, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                ReadGlobal(var) => {
                    self.chunk.write_opcode(OpCode::ReadGlobal, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                ReadGlobalArray(var) => {
                    self.chunk.write_opcode(OpCode::ReadGlobalArray, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                ReadGlobalArray2d(var) => {
                    self.chunk.write_opcode(OpCode::ReadGlobalArray2d, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                GetLocal(var) => {
                    self.chunk.write_opcode(OpCode::GetLocal, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                GetGlobal(var) => {
                    self.chunk.write_opcode(OpCode::GetGlobal, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                SetGlobal(var) => {
                    self.chunk.write_opcode(OpCode::SetGlobal, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                GetGlobalArray(var) => {
                    self.chunk.write_opcode(OpCode::GetGlobalArray, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                SetGlobalArray(var) => {
                    self.chunk.write_opcode(OpCode::SetGlobalArray, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                GetGlobalArray2d(var) => {
                    self.chunk.write_opcode(OpCode::GetGlobalArray2d, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                SetGlobalArray2d(var) => {
                    self.chunk.write_opcode(OpCode::SetGlobalArray2d, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                // TODO: locals
                InitArray(var) => {
                    self.chunk.write_opcode(OpCode::InitArray, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                InitArray2d(var) => {
                    self.chunk.write_opcode(OpCode::InitArray2d, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                SetArrayBound(var) => {
                    self.chunk.write_opcode(OpCode::SetArrayBound, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                SetArrayBound2d(var) => {
                    self.chunk.write_opcode(OpCode::SetArrayBound2d, line_no);
                    self.chunk.add_inline_operand(var, line_no);
                }
                PrintStart => {
                    self.chunk.write_opcode(OpCode::PrintStart, line_no);
                }
                PrintEnd => {
                    self.chunk.write_opcode(OpCode::PrintEnd, line_no);
                }
                PrintExpr => {
                    self.chunk.write_opcode(OpCode::PrintExpr, line_no);
                }
                PrintLabel(s) => {
                    self.chunk.write_opcode(OpCode::PrintLabel, line_no);
                    self.chunk.add_operand(s, line_no);
                }
                PrintAdvance3 => {
                    self.chunk.write_opcode(OpCode::PrintAdvance3, line_no);
                }
                PrintAdvance15 => {
                    self.chunk.write_opcode(OpCode::PrintAdvance15, line_no);
                }
                Dup => {
                    self.chunk.write_opcode(OpCode::Dup, line_no);
                }
                Negate => {
                    self.chunk.write_opcode(OpCode::Negate, line_no);
                }
                Not => {
                    self.chunk.write_opcode(OpCode::Not, line_no);
                }
                Add => {
                    self.chunk.write_opcode(OpCode::Add, line_no);
                }
                Sub => {
                    self.chunk.write_opcode(OpCode::Sub, line_no);
                }
                Mul => {
                    self.chunk.write_opcode(OpCode::Mul, line_no);
                }
                Div => {
                    self.chunk.write_opcode(OpCode::Div, line_no);
                }
                Pow => {
                    self.chunk.write_opcode(OpCode::Pow, line_no);
                }
                Equal => {
                    self.chunk.write_opcode(OpCode::Equal, line_no);
                }
                Less => {
                    self.chunk.write_opcode(OpCode::Less, line_no);
                }
                Greater => {
                    self.chunk.write_opcode(OpCode::Greater, line_no);
                }
                LoopTest => {
                    self.chunk.write_opcode(OpCode::LoopTest, line_no);
                }
                Noop => {
                    self.chunk.write_opcode(OpCode::Noop, line_no);
                }
            }

            Ok(())
        }
    }

}

pub mod disassembler {
    use std::io;

    use num_traits::FromPrimitive;

    use super::super::opcode::OpCode;
    use super::*;
    use crate::ast::function::Func;
    use crate::ast::*;

    use self::OpCode::*;

    //TODO: update Operand trait to eliminate &mut Chunk
    // requirement here
    pub struct Disassembler<'a, W> {
        chunk: &'a mut Chunk,
        ip: usize,
        line: usize,
        out: W,
        printing: bool,
    }

    impl<'a, W: io::Write> Disassembler<'a, W> {
        pub fn new(chunk: &'a mut Chunk, out: W) -> Self {
            Disassembler {
                chunk,
                ip: 0,
                line: usize::max_value(),
                out,
                printing: false,
            }
        }
        pub fn disassemble(&mut self) {
            while let Some(instr) = self.disassemble_instruction() {
                match instr {
                    Constant => self.disassemble_constant(),
                    Subroutine | Jump | JumpTrue | JumpFalse => {
                        self.disassemble_address()
                    }
                    CallNative | GetFunc | SetFunc => {
                        self.disassemble_function()
                    }

                    FnConstant => self.disassemble_function_id(),

                    GetGlobal | SetGlobal | GetLocal | SetLocal
                    | ReadGlobal | ReadGlobalArray | ReadGlobalArray2d
                    | GetGlobalArray | SetGlobalArray | GetGlobalArray2d
                    | SetGlobalArray2d | InitArray | InitArray2d
                    | SetArrayBound | SetArrayBound2d => {
                        self.disassemble_variable()
                    }

                    PrintLabel => self.disassemble_label(),
                    _ => {}
                }
                let _ = writeln!(&mut self.out);
            }

            let _ = writeln!(&mut self.out);
        }

        fn disassemble_constant(&mut self) {
            let n: f64 = self.get_operand();
            let _ = write!(&mut self.out, " {}", n);
        }
        fn disassemble_address(&mut self) {
            let p: JumpPoint = self.get_operand();
            let _ = write!(&mut self.out, " {}", p.0);
        }

        fn disassemble_label(&mut self) {
            let label: String = self.get_operand();
            let _ = write!(&mut self.out, " \"{}\"", label);
        }

        fn disassemble_variable(&mut self) {
            let var: Variable = self.get_inline_operand();
            let _ = write!(&mut self.out, " {}", var);
        }

        fn disassemble_function(&mut self) {
            let func: Func = self.get_inline_operand();
            let _ = write!(&mut self.out, " {}", func);
        }

        fn disassemble_function_id(&mut self) {
            let func: FuncId = self.get_inline_operand();
            let _ = write!(&mut self.out, " {}", func);
        }

        fn get_inline_operand<O: InlineOperand>(&mut self) -> O {
            let o = self.chunk.read_inline_operand(self.ip);
            self.ip += 2;
            o
        }

        fn get_operand<O: Operand>(&mut self) -> O {
            let o = self.chunk.read_operand(self.ip);
            self.ip += 2;
            o
        }

        fn disassemble_instruction(&mut self) -> Option<OpCode> {
            if self.ip >= self.chunk.code.len() {
                return None;
            }

            let line = self.chunk.line_no(self.ip);
            let byte = self.chunk.code[self.ip];

            let _ = if self.line == line {
                #[allow(clippy::write_literal)]
                write!(&mut self.out, "{} {:04}", " |   ", self.ip)
            } else {
                self.line = line;
                write!(&mut self.out, "{:<5} {:04}", line, self.ip)
            };

            self.ip += 1;

            OpCode::from_u8(byte).map(|instr| {
                match instr {
                    OpCode::PrintEnd => {
                        self.printing = false;
                    }
                    _ => {}
                }

                if self.printing {
                    let _ = write!(&mut self.out, "      {:8}", instr.short());
                } else {
                    let _ = write!(&mut self.out, "    {:10}", instr.short());
                }

                match instr {
                    OpCode::PrintStart => {
                        self.printing = true;
                    }
                    _ => {}
                }

                instr
            })
        }
    }
}
