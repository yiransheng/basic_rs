use std::io::{Read, Write};

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use int_hash::IntHashMap;
use num_traits::{FromPrimitive, ToPrimitive};

use super::line_mapping::LineMapping;
use super::opcode::OpCode;
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
    constants: Vec<f64>,
    jump_points: Vec<JumpPoint>,
    strings: Vec<String>,
    line_map: LineMapping,

    user_fns: IntHashMap<Func, Chunk>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            jump_points: Vec::new(),
            strings: Vec::new(),
            line_map: LineMapping::new(),

            user_fns: IntHashMap::default(),
        }
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
        self.line_map.push_line(line);
    }
    pub fn add_function(&mut self, func: Func, chunk: Chunk) {
        let p: [u8; 2] = func.into();
        self.user_fns.insert(func, chunk);
    }
    #[inline(always)]
    pub fn get_function(&mut self, func: &Func) -> Option<&mut Chunk> {
        self.user_fns.get_mut(func)
    }

    //TODO: this should return Result
    pub fn add_operand<O: Operand>(&mut self, o: O, line: usize) -> u16 {
        let slot = o.add_to_chunk(self);
        self.line_map.push_line(line);
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
    pub fn read_operand_ref<O: Operand>(&mut self, offset: usize) -> &O {
        O::read_ref_from_chunk(offset, self)
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
