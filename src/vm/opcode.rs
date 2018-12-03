pub const OP_CONSTANT: u8 = 0x00;
pub const OP_CONSTANT2: u8 = 0x01;

pub const OP_RETURN: u8 = 0x02;
pub const OP_JUMP: u8 = 0x03;
pub const OP_COND_JUMP: u8 = 0x04;
pub const OP_SUBROUTINE: u8 = 0x05;
pub const OP_CALL_NATIVE: u8 = 0x06;
pub const OP_CALL: u8 = 0x07;

pub const OP_POP: u8 = 0x0a;
pub const OP_GET_GLOBAL: u8 = 0x0b;
pub const OP_SET_GLOBAL: u8 = 0x0c;

pub const OP_GET_GLOBAL_ARRAY: u8 = 0x0d;
pub const OP_SET_GLOBAL_ARRAY: u8 = 0x0e;

pub const OP_GET_GLOBAL_ARRAY2: u8 = 0x0f;
pub const OP_SET_GLOBAL_ARRAY2: u8 = 0x10;

pub const OP_PRINT: u8 = 0x11;

pub const OP_NEGATE: u8 = 0x20;
pub const OP_NOT: u8 = 0x21;
pub const OP_ADD: u8 = 0x22;
pub const OP_SUBTRACT: u8 = 0x23;
pub const OP_MULTIPLY: u8 = 0x24;
pub const OP_DIVIDE: u8 = 0x25;
pub const OP_POWER: u8 = 0x26;

pub const OP_EQUAL: u8 = 0x27;
pub const OP_GREATER: u8 = 0x28;
pub const OP_LESS: u8 = 0x29;
