use rustc_hash::FxHashMap;
use slotmap::SecondaryMap;
use std::ops::Deref;

use crate::ast::Func;
use crate::ir::*;
use crate::vm::{Chunk, FuncId, FuncIdGen, JumpPoint, LocalVar, OpCode};

#[derive(Debug, Copy, Clone)]
pub struct WriteError(pub &'static str);

struct ChunkWriter<'a> {
    func_map: &'a SecondaryMap<FunctionName, FuncId>,
    chunk: &'a mut Chunk,
    strings: &'a str,

    jp_label_map: SecondaryMap<Label, JumpPoint>,
    jp_indices: Vec<(u16, Label)>,
}

impl<'a> ChunkWriter<'a> {
    fn mark_jump_point(&mut self, label: Label) {
        self.jp_label_map.insert(label, JumpPoint(self.chunk.len()));
    }
    fn get_string_label(&self, offset: usize, len: usize) -> &str {
        ::std::str::from_utf8(&self.strings.as_bytes()[offset..offset + len])
            .unwrap()
    }
}

trait ChunkWrite {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError>;
}

pub fn codegen(
    ir: &Program,
) -> Result<(FuncId, FxHashMap<FuncId, Chunk>), WriteError> {
    let mut func_id_gen = FuncIdGen::new();
    let func_map: SecondaryMap<FunctionName, FuncId> = ir
        .functions
        .iter()
        .map(move |function| {
            let id = func_id_gen.next_id();
            (function.name, id)
        })
        .collect();

    let mut compiled_functions: FxHashMap<FuncId, Chunk> = FxHashMap::default();

    for function in &ir.functions {
        let mut chunk = Chunk::new();
        let id = func_map.get(function.name).cloned().unwrap();
        let mut writer = ChunkWriter {
            func_map: &func_map,
            strings: &ir.labels,
            chunk: &mut chunk,
            jp_label_map: SecondaryMap::new(),
            jp_indices: vec![],
        };
        function.write(&mut writer)?;
        compiled_functions.insert(id, chunk);
    }

    let main_id = func_map.get(ir.main).cloned().unwrap();

    Ok((main_id, compiled_functions))
}

impl ChunkWrite for Function {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        let n_locals = self.locals.len();
        writer.chunk.write_opcode(OpCode::DeclLocal);
        writer.chunk.write(n_locals as u8);

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
            writer.chunk.write_opcode(OpCode::ReturnValue);
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
                    writer.chunk.write_opcode(OpCode::BindFunc);
                    writer.chunk.add_inline_operand(*func);
                    writer.chunk.add_inline_operand(
                        writer.func_map.get(*fname).cloned().unwrap(),
                    );
                }
                _ => {}
            },
            Statement::CallSub(fname) => {
                writer.chunk.write_opcode(OpCode::Call);
                writer.chunk.add_inline_operand(
                    writer.func_map.get(*fname).cloned().unwrap(),
                );
                writer.chunk.write(0); // n args
            }
            Statement::Print(expr) => {
                expr.write(writer)?;
                writer.chunk.write_opcode(OpCode::PrintExpr);
            }
            Statement::PrintLabel(offset, len) => {
                writer.chunk.write_opcode(OpCode::PrintLabel);
                writer.chunk.add_operand(
                    writer.get_string_label(*offset, *len).to_owned(),
                );
            }
            Statement::PrintAdvance15 => {
                writer.chunk.write_opcode(OpCode::PrintAdvance15);
            }
            Statement::PrintAdvance3 => {
                writer.chunk.write_opcode(OpCode::PrintAdvance3);
            }
            Statement::PrintNewline => {
                writer.chunk.write_opcode(OpCode::PrintNewline);
            }
            _ => unimplemented!(),
        }

        Ok(())
    }
}

impl ChunkWrite for Expr {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        match self {
            Expr::ReadData => {
                writer.chunk.write_opcode(OpCode::Read);
            }
            Expr::Const(n) => {
                writer.chunk.write_opcode(OpCode::Constant);
                writer.chunk.add_operand(*n);
            }
            Expr::RandF64 => {
                writer.chunk.write_opcode(OpCode::Rand);
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
                    writer.chunk.write(1); // 1 arg
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
