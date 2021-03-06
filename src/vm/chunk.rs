use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num_traits::{FromPrimitive, ToPrimitive};

use super::line_mapping::LineMapping;
use super::opcode::OpCode;
use super::value::FuncId;
use crate::ast::function::Func;
use crate::ast::variable::Variable;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct JumpPoint(pub usize);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct LocalVar(pub u16);

pub trait InlineOperand: Into<[u8; 2]> {
    fn from_bytes_unchecked(bytes: [u8; 2]) -> Self;
}

pub trait Operand: Clone {
    fn storage_mut(chunk: &mut Chunk) -> &mut Vec<Self>
    where
        Self: Sized;

    fn storage(chunk: &Chunk) -> &[Self]
    where
        Self: Sized;

    fn add_to_chunk(self, chunk: &mut Chunk) -> u16
    where
        Self: Sized,
    {
        let where_to = Self::storage_mut(chunk);
        let index = where_to.len();
        assert!(index < (u16::max_value() as usize));

        where_to.push(self);

        let index = index as u16;

        chunk.write_index(index);

        index
    }

    fn read_from_chunk(offset: usize, chunk: &Chunk) -> Self
    where
        Self: Sized,
    {
        let index = chunk.read_index(offset) as usize;
        let where_to = Self::storage(chunk);

        where_to[index].clone()
    }

    fn read_ref_from_chunk(offset: usize, chunk: &Chunk) -> &Self
    where
        Self: Sized,
    {
        let index = chunk.read_index(offset) as usize;
        let where_to = Self::storage(chunk);

        &where_to[index]
    }
}

impl Operand for JumpPoint {
    fn storage_mut(chunk: &mut Chunk) -> &mut Vec<Self> {
        &mut chunk.jump_points
    }
    fn storage(chunk: &Chunk) -> &[Self] {
        &chunk.jump_points
    }
}

impl Operand for f64 {
    fn storage_mut(chunk: &mut Chunk) -> &mut Vec<Self> {
        &mut chunk.constants
    }
    fn storage(chunk: &Chunk) -> &[Self] {
        &chunk.constants
    }
}

impl Operand for String {
    fn storage_mut(chunk: &mut Chunk) -> &mut Vec<Self> {
        &mut chunk.strings
    }
    fn storage(chunk: &Chunk) -> &[Self] {
        &chunk.strings
    }
}

pub struct Chunk {
    code: Vec<u8>,

    constants: Vec<f64>,

    jump_points: Vec<JumpPoint>,
    strings: Vec<String>,
    line_map: LineMapping,
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),

            constants: Vec::new(),

            jump_points: Vec::new(),
            strings: Vec::new(),
            line_map: LineMapping::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn write_opcode(&mut self, code: OpCode, line: usize) {
        self.write(code as u8, line)
    }

    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
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
        let storage = O::storage_mut(self);
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
    pub fn read_operand<O: Operand>(&self, offset: usize) -> O {
        O::read_from_chunk(offset, self)
    }

    #[inline(always)]
    pub fn read_operand_ref<O: Operand>(&self, offset: usize) -> &O {
        O::read_ref_from_chunk(offset, self)
    }

    #[inline(always)]
    pub fn read_inline_operand<O: InlineOperand>(&self, offset: usize) -> O {
        let bytes = [self.read_byte(offset), self.read_byte(offset + 1)];
        O::from_bytes_unchecked(bytes)
    }

    fn write_index(&mut self, index: u16) {
        self.code.write_u16::<LittleEndian>(index).unwrap();
    }

    fn read_index(&self, offset: usize) -> u16 {
        (&self.code[offset..]).read_u16::<LittleEndian>().unwrap()
    }
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

impl From<usize> for LocalVar {
    fn from(i: usize) -> Self {
        LocalVar(i as u16)
    }
}

impl Into<[u8; 2]> for LocalVar {
    fn into(self) -> [u8; 2] {
        let b = self.0;
        let blo = (b & 0xff) as u8;
        let bhi = ((b >> 8) & 0xff) as u8;
        [blo, bhi]
    }
}
impl Into<usize> for LocalVar {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl InlineOperand for LocalVar {
    #[inline(always)]
    fn from_bytes_unchecked(bytes: [u8; 2]) -> Self {
        let [blo, bhi] = bytes;
        let blo = blo as u16;
        let bhi = bhi as u16;
        LocalVar((bhi << 8) + blo)
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
        chunk: &'a Chunk,
        ip: usize,
        line: usize,
        out: W,
    }

    impl<'a, W: io::Write> Disassembler<'a, W> {
        pub fn new(chunk: &'a Chunk, out: W) -> Self {
            Disassembler {
                chunk,
                ip: 0,
                line: usize::max_value(),
                out,
            }
        }
        pub fn disassemble(&mut self) {
            while let Some(instr) = self.disassemble_instruction() {
                match instr {
                    Constant => self.disassemble_constant(),
                    Jump | JumpTrue | JumpFalse => self.disassemble_address(),
                    CallNative | GetFunc => self.disassemble_function(),

                    BindFunc => {
                        self.disassemble_function();
                        self.disassemble_function_id();
                    }
                    Call => {
                        self.disassemble_function_id();
                        let _ = write!(&mut self.out, " args:");
                        self.disassemble_count();
                    }
                    CallIndirect => {
                        let _ = write!(&mut self.out, " args:");
                        self.disassemble_count();
                    }

                    DeclLocal => {
                        self.disassemble_count();
                    }

                    GetLocal | SetLocal => {
                        self.disassemble_local();
                    }

                    GetGlobal | SetGlobal | GetGlobalArray1d | DefineDim1d
                    | DefineDim2d | SetGlobalArray1d | GetGlobalArray2d
                    | SetGlobalArray2d | InitArray1d | InitArray2d => {
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

        fn disassemble_count(&mut self) {
            let n = self.chunk.read_byte(self.ip);
            let _ = write!(&mut self.out, " {}", n);
            self.ip += 1;
        }

        fn disassemble_address(&mut self) {
            let p: JumpPoint = self.get_operand();
            let _ = write!(&mut self.out, " {}", p.0);
        }

        fn disassemble_label(&mut self) {
            let label: String = self.get_operand();
            let _ = write!(&mut self.out, " \"{}\"", label);
        }

        fn disassemble_local(&mut self) {
            let var: LocalVar = self.get_inline_operand();
            let _ = write!(&mut self.out, " ${}", var.0);
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
                let _ = write!(&mut self.out, "    {:10}", instr.short());

                instr
            })
        }
    }
}
