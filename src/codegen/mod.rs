use slotmap::SecondaryMap;
use std::ops::Deref;

use crate::ast::Func;
use crate::ir::*;
use crate::vm::{Chunk, FuncId, JumpPoint, LocalVar, OpCode};

struct CodeGen<'a> {
    chunk: &'a mut Chunk,
    ir: &'a Program,

    jp_label_map: SecondaryMap<Label, JumpPoint>,
    func_map: SecondaryMap<FunctionName, FuncId>,
    jp_indices: Vec<(u16, Label)>,
}

#[derive(Debug, Copy, Clone)]
struct WriteError(pub &'static str);

trait ChunkWrite {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError>;
}

impl ChunkWrite for Statement {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        match self {
            Statement::Assign(lval, expr) => match lval {
                LValue::Global(var) => {
                    expr.write(writer)?;
                    writer.chunk.write_opcode(OpCode::SetGlobal);
                    writer.chunk.add_inline_operand(*var);
                }
                LValue::Local(index) => {
                    expr.write(writer)?;
                    writer.chunk.write_opcode(OpCode::SetLocal);
                    writer.chunk.add_inline_operand(LocalVar::from(*index));
                }
                LValue::FnPtr(_) => return Err(WriteError("Type error")),
                LValue::ArrPtr(var, offset) => match offset {
                    Offset::OneD(i) => {
                        expr.write(writer)?;
                        i.write(writer)?;
                        writer.chunk.write_opcode(OpCode::SetGlobalArray);
                        writer.chunk.add_inline_operand(*var);
                    }
                    Offset::TwoD(i, j) => {
                        expr.write(writer)?;
                        i.write(writer)?;
                        j.write(writer)?;
                        writer.chunk.write_opcode(OpCode::SetGlobalArray2d);
                        writer.chunk.add_inline_operand(*var);
                    }
                },
            },
            Statement::DefFn(lval, fname) => match lval {
                LValue::FnPtr(func) => {
                    writer.chunk.write_opcode(OpCode::FnConstant);
                    writer.chunk.add_inline_operand(
                        writer.func_map.get(*fname).cloned().unwrap(),
                    );
                    writer.chunk.write_opcode(OpCode::SetFunc);
                    writer.chunk.add_inline_operand(*func);
                }
                _ => {}
            },
            Statement::CallSub(fname) => {
                writer.chunk.write_opcode(OpCode::Call);
                writer.chunk.add_inline_operand(
                    writer.func_map.get(*fname).cloned().unwrap(),
                );
            }
            _ => unimplemented!(),
        }

        Ok(())
    }
}

impl ChunkWrite for Expr {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        match self {
            Expr::ReadData => unimplemented!(),
            Expr::Const(n) => {
                writer.chunk.write_opcode(OpCode::Constant);
                writer.chunk.add_operand(*n);
            }
            Expr::RandF64 => {
                // TODO: add OpCode for random instead of call native
                writer.chunk.write_opcode(OpCode::CallNative);
                writer.chunk.add_inline_operand(Func::Rnd);
            }
            Expr::Get(lval) => match lval.deref() {
                LValue::Global(var) => {
                    writer.chunk.write_opcode(OpCode::GetGlobal);
                    writer.chunk.add_inline_operand(*var);
                }
                LValue::Local(index) => {
                    writer.chunk.write_opcode(OpCode::GetLocal);
                    writer.chunk.add_inline_operand(LocalVar::from(*index));
                }
                LValue::FnPtr(func) => {
                    writer.chunk.write_opcode(OpCode::GetFunc);
                    writer.chunk.add_inline_operand(*func);
                }
                LValue::ArrPtr(var, offset) => match offset {
                    Offset::OneD(i) => {
                        i.write(writer)?;
                        writer.chunk.write_opcode(OpCode::GetGlobalArray);
                        writer.chunk.add_inline_operand(*var);
                    }
                    Offset::TwoD(i, j) => {
                        i.write(writer)?;
                        j.write(writer)?;
                        writer.chunk.write_opcode(OpCode::GetGlobalArray2d);
                        writer.chunk.add_inline_operand(*var);
                    }
                },
            },
            Expr::Call(lval, expr) => match lval.deref() {
                LValue::FnPtr(func) => {
                    expr.write(writer)?;
                    writer.chunk.write_opcode(OpCode::GetFunc);
                    writer.chunk.add_inline_operand(*func);
                    writer.chunk.write_opcode(OpCode::CallIndirect);
                }
                _ => return Err(WriteError("Type error")),
            },
            Expr::Unary(op, expr) => {
                expr.write(writer)?;
                match op {
                    UnaryOp::Not => {
                        writer.chunk.write_opcode(OpCode::Not);
                    }
                    UnaryOp::Neg => {
                        writer.chunk.write_opcode(OpCode::Negate);
                    }
                    UnaryOp::Sin => {
                        writer.chunk.write_opcode(OpCode::CallNative);
                        writer.chunk.add_inline_operand(Func::Sin);
                    }
                    UnaryOp::Cos => {
                        writer.chunk.write_opcode(OpCode::CallNative);
                        writer.chunk.add_inline_operand(Func::Cos);
                    }
                    UnaryOp::Atn => {
                        writer.chunk.write_opcode(OpCode::CallNative);
                        writer.chunk.add_inline_operand(Func::Atn);
                    }
                    UnaryOp::Exp => {
                        writer.chunk.write_opcode(OpCode::CallNative);
                        writer.chunk.add_inline_operand(Func::Exp);
                    }
                    UnaryOp::Abs => {
                        writer.chunk.write_opcode(OpCode::CallNative);
                        writer.chunk.add_inline_operand(Func::Abs);
                    }
                    UnaryOp::Log => {
                        writer.chunk.write_opcode(OpCode::CallNative);
                        writer.chunk.add_inline_operand(Func::Log);
                    }
                    UnaryOp::Sqr => {
                        writer.chunk.write_opcode(OpCode::CallNative);
                        writer.chunk.add_inline_operand(Func::Sqr);
                    }
                    UnaryOp::Trunc => {
                        writer.chunk.write_opcode(OpCode::CallNative);
                        writer.chunk.add_inline_operand(Func::Int);
                    }
                }
            }
            Expr::Binary(op, lhs, rhs) => {
                lhs.write(writer)?;
                rhs.write(writer)?;
                match op {
                    BinaryOp::Add => {
                        writer.chunk.write_opcode(OpCode::Add);
                    }
                    BinaryOp::Sub => {
                        writer.chunk.write_opcode(OpCode::Sub);
                    }
                    BinaryOp::Mul => {
                        writer.chunk.write_opcode(OpCode::Mul);
                    }
                    BinaryOp::Div => {
                        writer.chunk.write_opcode(OpCode::Div);
                    }
                    BinaryOp::Pow => {
                        writer.chunk.write_opcode(OpCode::Pow);
                    }
                    BinaryOp::Less => {
                        writer.chunk.write_opcode(OpCode::Less);
                    }
                    BinaryOp::Greater => {
                        writer.chunk.write_opcode(OpCode::Greater);
                    }
                    BinaryOp::Equal => {
                        writer.chunk.write_opcode(OpCode::Equal);
                    }
                    BinaryOp::CopySign => {
                        // writer.chunk.write_opcode(OpCode::Pow);
                        unimplemented!()
                    }
                }
            }
        }
        Ok(())
    }
}

struct ChunkWriter {
    chunk: Chunk,

    jp_label_map: SecondaryMap<Label, JumpPoint>,
    func_map: SecondaryMap<FunctionName, FuncId>,
    jp_indices: Vec<(u16, Label)>,
}

impl<'a> CodeGen<'a> {
    fn write_statement(&mut self, stmt: &Statement) {}

    fn write_expr(&mut self, expr: &Expr) {}

    fn mark_jump_point(&mut self, label: Label) {
        let codelen = self.chunk.len();
        self.jp_label_map.insert(label, JumpPoint(codelen));
    }
}
