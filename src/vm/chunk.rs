use std::io::{Read, Write};

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

use super::line_mapping::LineMapping;
use super::opcode::OpCode;
use crate::ast::variable::Variable;

#[derive(Copy, Clone, Eq, PartialEq)]
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

pub trait Operand: Clone {
    fn storage(chunk: &mut Chunk) -> &mut Vec<Self>
    where
        Self: Sized;

    fn add_to_chunk(self, chunk: &mut Chunk)
    where
        Self: Sized,
    {
        let where_to = Self::storage(chunk);
        let index = where_to.len();
        assert!(index < (u16::max_value() as usize));

        where_to.push(self);

        chunk.write_index(index as u16);
    }

    fn read_from_chunk(offset: usize, chunk: &mut Chunk) -> Self
    where
        Self: Sized,
    {
        let index = chunk.read_index(offset) as usize;
        let where_to = Self::storage(chunk);

        where_to[index].clone()
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

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<f64>,
    jump_points: Vec<JumpPoint>,
    strings: Vec<String>,
    line_map: LineMapping,
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

    #[inline(always)]
    pub fn write_opcode(&mut self, code: OpCode, line: usize) {
        self.write(code as u8, line)
    }
    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte.into());
        self.line_map.push_line(line);
    }

    pub fn add_operand<O: Operand>(&mut self, o: O, line: usize) {
        o.add_to_chunk(self);
        self.line_map.push_line(line);
    }

    pub fn add_inline_oprerand<O: InlineOperand>(&mut self, o: O, line: usize) {
        let bytes = o.into();
        self.write(bytes[0], line);
        self.write(bytes[1], line);
        self.line_map.push_line(line);
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
    pub fn read_inline_operand<O: InlineOperand>(&mut self, offset: usize) -> O {
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
