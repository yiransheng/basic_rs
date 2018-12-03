use super::line_mapping::LineMapping;
use super::opcode::*;
use super::value::{Static, Variant};

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Static>,
    strings: Vec<String>,
    line_map: LineMapping,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            strings: Vec::new(),
            line_map: LineMapping::new(),
        }
    }

    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.line_map.push_line(line);
    }

    pub fn write_constant(&mut self, constant: Variant, line: usize) -> usize {
        let const_len = self.constants.len();

        if const_len < (u8::max_value() as usize) {
            self.write(OP_CONSTANT, line);
            let offset = self.add_constant(constant);
            self.write(offset as u8, line);
            offset
        } else if (const_len < (u16::max_value() as usize)) {
            self.write(OP_CONSTANT2, line);
            let offset = self.add_constant(constant);
            self.write(((offset >> 8) & 0xff) as u8, line);
            self.write((offset & 0xff) as u8, line);
            offset
        } else {
            panic!("Too many constants");
        }
    }

    pub fn write_label(&mut self, label: String, line: usize) {
        assert!(self.strings.len() < (u8::max_value() as usize));

        let offset = self.strings.len();
        self.strings.push(label);
        let v = Variant::Label(offset as u8);

        self.write_constant(v, line);
    }

    pub fn read_byte(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub fn read_constant(&self, offset: usize) -> Variant {
        let constant_offset = self.read_byte(offset);
        (&self.constants[constant_offset as usize]).clone().into()
    }

    pub fn add_constant<T: Into<Static>>(&mut self, value: T) -> usize {
        self.constants.push(value.into());
        self.constants.len() - 1
    }
}
