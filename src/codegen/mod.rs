use slotmap::SecondaryMap;
use std::ops::Deref;

use crate::ast::Func;
use crate::ir::*;
use crate::vm::{Chunk, FuncId, JumpPoint, LocalVar, OpCode};

#[derive(Debug, Copy, Clone)]
struct WriteError(pub &'static str);

struct ChunkWriter {
    chunk: Chunk,

    jp_label_map: SecondaryMap<Label, JumpPoint>,
    func_map: SecondaryMap<FunctionName, FuncId>,
    jp_indices: Vec<(u16, Label)>,
}

impl ChunkWriter {
    fn mark_jump_point(&mut self, label: Label) {
        self.jp_label_map.insert(label, JumpPoint(self.chunk.len()));
    }
}

trait ChunkWrite {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError>;
}

impl ChunkWrite for Function {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        let block_pairs = self
            .iter()
            .zip(self.iter().skip(1).map(Option::Some).chain(Some(None)));

        for (current_block, next_block) in block_pairs {
            let current_label = current_block.label;
            let next_label = next_block.map(|b| b.label);

            writer.mark_jump_point(current_label);
            current_block.write(writer)?;

            write_block_exit(&current_block.exit, next_label, writer)?;
        }

        for (index, label) in &writer.jp_indices {
            let jp = writer
                .jp_label_map
                .get(*label)
                .cloned()
                .ok_or_else(|| WriteError("Jumps to nowhere"))?;

            writer.chunk.set_operand(*index, jp);
        }

        Ok(())
    }
}

fn write_block_exit(
    exit: &BlockExit,
    next_label: Option<Label>,
    writer: &mut ChunkWriter,
) -> Result<(), WriteError> {
    match exit {
        BlockExit::Jump(label) if Some(*label) == next_label => {}
        BlockExit::Jump(label) => {
            writer.chunk.write_opcode(OpCode::Jump);
            let jp = JumpPoint(0);
            let index = writer.chunk.add_operand(jp);
            writer.jp_indices.push((index, *label));
        }
        BlockExit::Return(None) => {
            writer.chunk.write_opcode(OpCode::Return);
        }
        BlockExit::Return(Some(expr)) => {
            expr.write(writer)?;
            writer.chunk.write_opcode(OpCode::Return);
        }
        BlockExit::Switch(cond, true_br, None) => {
            cond.write(writer)?;
            writer.chunk.write_opcode(OpCode::JumpTrue);
            let jp = JumpPoint(0);
            let index = writer.chunk.add_operand(jp);
            writer.jp_indices.push((index, *true_br));
        }
        BlockExit::Switch(cond, true_br, Some(false_br)) => {
            cond.write(writer)?;
            writer.chunk.write_opcode(OpCode::JumpTrue);
            let jp = JumpPoint(0);
            let index = writer.chunk.add_operand(jp);
            writer.jp_indices.push((index, *true_br));

            write_block_exit(&BlockExit::Jump(*false_br), next_label, writer)?;
        }
    }

    Ok(())
}

impl ChunkWrite for BasicBlock {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        for stmt in &self.statements {
            stmt.write(writer)?;
        }
        Ok(())
    }
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
