use slotmap::SecondaryMap;
use std::ops::Deref;

use crate::ast::Func;
use crate::ir::*;
use crate::vm::{Chunk, JumpPoint, LocalVar, OpCode};

struct CodeGen<'a> {
    chunk: &'a mut Chunk,
    ir: &'a Program,

    jp_label_map: SecondaryMap<Label, JumpPoint>,
    jp_indices: Vec<(u16, Label)>,
}

impl<'a> CodeGen<'a> {
    fn write_statement(&mut self, stmt: &Statement) {}

    fn write_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::ReadData => unimplemented!(),
            Expr::Const(n) => {
                self.chunk.write_opcode(OpCode::Constant);
                self.chunk.add_operand(*n);
            }
            Expr::RandF64 => {
                self.chunk.write_opcode(OpCode::CallNative);
                self.chunk.add_inline_operand(Func::Rnd);
            }
            Expr::Get(lval) => match lval.deref() {
                LValue::Global(var) => {
                    self.chunk.write_opcode(OpCode::GetGlobal);
                    self.chunk.add_inline_operand(*var);
                }
                LValue::Local(index) => {
                    self.chunk.write_opcode(OpCode::GetGlobal);
                    self.chunk.add_inline_operand(LocalVar::from(*index));
                }
                LValue::FnPtr(func) => {
                    self.chunk.write_opcode(OpCode::GetFunc);
                    self.chunk.add_inline_operand(*func);
                }
                LValue::ArrPtr(var, offset) => match offset {
                    Offset::OneD(i) => {
                        self.write_expr(i);
                        self.chunk.write_opcode(OpCode::GetGlobalArray);
                        self.chunk.add_inline_operand(*var);
                    }
                    Offset::TwoD(i, j) => {
                        self.write_expr(i);
                        self.write_expr(j);
                        self.chunk.write_opcode(OpCode::GetGlobalArray2d);
                        self.chunk.add_inline_operand(*var);
                    }
                },
            },
            Expr::Call(lval, expr) => match lval.deref() {
                LValue::FnPtr(func) => {
                    self.write_expr(expr);
                    self.chunk.write_opcode(OpCode::GetFunc);
                    self.chunk.add_inline_operand(*func);
                    self.chunk.write_opcode(OpCode::Call);
                }
                _ => return,
            },
            Expr::Unary(op, expr) => {
                self.write_expr(expr);
                match op {
                    UnaryOp::Not => {
                        self.chunk.write_opcode(OpCode::Not);
                    }
                    UnaryOp::Neg => {
                        self.chunk.write_opcode(OpCode::Negate);
                    }
                    UnaryOp::Sin => {
                        self.chunk.write_opcode(OpCode::CallNative);
                        self.chunk.add_inline_operand(Func::Sin);
                    }
                    UnaryOp::Cos => {
                        self.chunk.write_opcode(OpCode::CallNative);
                        self.chunk.add_inline_operand(Func::Cos);
                    }
                    UnaryOp::Atn => {
                        self.chunk.write_opcode(OpCode::CallNative);
                        self.chunk.add_inline_operand(Func::Atn);
                    }
                    UnaryOp::Exp => {
                        self.chunk.write_opcode(OpCode::CallNative);
                        self.chunk.add_inline_operand(Func::Exp);
                    }
                    UnaryOp::Abs => {
                        self.chunk.write_opcode(OpCode::CallNative);
                        self.chunk.add_inline_operand(Func::Abs);
                    }
                    UnaryOp::Log => {
                        self.chunk.write_opcode(OpCode::CallNative);
                        self.chunk.add_inline_operand(Func::Log);
                    }
                    UnaryOp::Sqr => {
                        self.chunk.write_opcode(OpCode::CallNative);
                        self.chunk.add_inline_operand(Func::Sqr);
                    }
                    UnaryOp::Trunc => {
                        self.chunk.write_opcode(OpCode::CallNative);
                        self.chunk.add_inline_operand(Func::Int);
                    }
                }
            }
            Expr::Binary(op, lhs, rhs) => {
                self.write_expr(lhs);
                self.write_expr(rhs);
                match op {
                    BinaryOp::Add => {
                        self.chunk.write_opcode(OpCode::Add);
                    }
                    BinaryOp::Sub => {
                        self.chunk.write_opcode(OpCode::Sub);
                    }
                    BinaryOp::Mul => {
                        self.chunk.write_opcode(OpCode::Mul);
                    }
                    BinaryOp::Div => {
                        self.chunk.write_opcode(OpCode::Div);
                    }
                    BinaryOp::Pow => {
                        self.chunk.write_opcode(OpCode::Pow);
                    }
                    BinaryOp::Less => {
                        self.chunk.write_opcode(OpCode::Less);
                    }
                    BinaryOp::Greater => {
                        self.chunk.write_opcode(OpCode::Greater);
                    }
                    BinaryOp::Equal => {
                        self.chunk.write_opcode(OpCode::Equal);
                    }
                    BinaryOp::CopySign => {
                        // self.chunk.write_opcode(OpCode::Pow);
                        unimplemented!()
                    }
                }
            }
        }
    }

    fn mark_jump_point(&mut self, label: Label) {
        let codelen = self.chunk.len();
        self.jp_label_map.insert(label, JumpPoint(codelen));
    }
}
