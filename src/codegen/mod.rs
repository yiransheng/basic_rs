use std::error::Error;
use std::fmt;
use std::ops::Deref;

use rustc_hash::FxHashMap;
use slotmap::SecondaryMap;

use crate::ast::{Func, LineNo};
use crate::ir::*;
use crate::vm::{
    Chunk, FuncId, FuncIdGen, InlineOperand, JumpPoint, LocalVar, OpCode,
    Operand, VM,
};

#[derive(Debug, Copy, Clone)]
pub struct WriteError(pub &'static str);

impl fmt::Display for WriteError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for WriteError {
    fn description(&self) -> &str {
        "Codegen error"
    }
}

struct ChunkWriter<'a> {
    func_map: &'a SecondaryMap<FunctionName, FuncId>,
    chunk: &'a mut Chunk,
    line_no: LineNo,
    strings: &'a str,

    jp_label_map: SecondaryMap<Label, JumpPoint>,
    jp_indices: Vec<(u16, Label)>,
}

impl<'a> ChunkWriter<'a> {
    fn mark_jump_point(&mut self, label: Label) {
        self.jp_label_map.insert(label, JumpPoint(self.chunk.len()));
    }
    fn set_line_no(&mut self, line_no: LineNo) {
        self.line_no = line_no;
    }
    fn write_opcode(&mut self, opcode: OpCode) {
        self.chunk.write_opcode(opcode, self.line_no);
    }
    fn write(&mut self, byte: u8) {
        self.chunk.write(byte, self.line_no);
    }
    fn add_operand<O: Operand>(&mut self, o: O) -> u16 {
        self.chunk.add_operand(o, self.line_no)
    }
    fn add_inline_operand<O: InlineOperand>(&mut self, o: O) {
        self.chunk.add_inline_operand(o, self.line_no)
    }
    fn get_string_label(&self, offset: usize, len: usize) -> &str {
        ::std::str::from_utf8(&self.strings.as_bytes()[offset..offset + len])
            .unwrap()
    }
}

trait ChunkWrite {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError>;
}

pub fn codegen(ir: &Program) -> Result<VM, WriteError> {
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
        if function.name == ir.main {
            for global in &ir.globals {
                match global {
                    GlobalKind::ArrPtr(var, dim) => match dim {
                        Offset::OneD(..) => {
                            chunk.write_opcode(OpCode::InitArray1d, 0);
                            chunk.add_inline_operand(*var, 0);
                        }
                        Offset::TwoD(..) => {
                            chunk.write_opcode(OpCode::InitArray2d, 0);
                            chunk.add_inline_operand(*var, 0);
                        }
                    },
                    _ => {}
                }
            }
        }

        let id = func_map.get(function.name).cloned().unwrap();
        let mut writer = ChunkWriter {
            func_map: &func_map,
            line_no: LineNo::default(),
            strings: &ir.labels,
            chunk: &mut chunk,
            jp_label_map: SecondaryMap::new(),
            jp_indices: vec![],
        };
        writer.set_line_no(function.line_no);
        function.write(&mut writer)?;
        compiled_functions.insert(id, chunk);
    }

    let main_id = func_map.get(ir.main).cloned().unwrap();

    Ok(VM::new(main_id, compiled_functions, ir.data.clone()))
}

impl ChunkWrite for Function {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        let n_locals = self.locals.len();
        writer.write_opcode(OpCode::DeclLocal);
        writer.write(n_locals as u8);

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
            writer.write_opcode(OpCode::Jump);
            let jp = JumpPoint(0);
            let index = writer.add_operand(jp);
            writer.jp_indices.push((index, *label));
        }
        BlockExit::Return(None) => {
            writer.write_opcode(OpCode::Return);
        }
        BlockExit::Return(Some(expr)) => {
            expr.write(writer)?;
            writer.write_opcode(OpCode::ReturnValue);
        }
        BlockExit::Switch(cond, true_br, None) => {
            cond.write(writer)?;
            writer.write_opcode(OpCode::JumpTrue);
            let jp = JumpPoint(0);
            let index = writer.add_operand(jp);
            writer.jp_indices.push((index, *true_br));
        }
        BlockExit::Switch(cond, true_br, Some(false_br)) => {
            cond.write(writer)?;
            writer.write_opcode(OpCode::JumpTrue);
            let jp = JumpPoint(0);
            let index = writer.add_operand(jp);
            writer.jp_indices.push((index, *true_br));

            write_block_exit(&BlockExit::Jump(*false_br), next_label, writer)?;
        }
    }

    Ok(())
}

impl ChunkWrite for BasicBlock {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        for (stmt, line_no) in self.statements.iter().zip(self.line_nos.iter())
        {
            writer.set_line_no(*line_no);
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
                    writer.write_opcode(OpCode::SetGlobal);
                    writer.add_inline_operand(*var);
                }
                LValue::Local(index) => {
                    expr.write(writer)?;
                    writer.write_opcode(OpCode::SetLocal);
                    writer.add_inline_operand(LocalVar::from(*index));
                }
                LValue::FnPtr(_) => return Err(WriteError("Type error")),
                LValue::ArrPtr(var, offset) => match offset {
                    Offset::OneD(i) => {
                        expr.write(writer)?;
                        i.write(writer)?;
                        writer.write_opcode(OpCode::SetGlobalArray1d);
                        writer.add_inline_operand(*var);
                    }
                    Offset::TwoD(i, j) => {
                        expr.write(writer)?;
                        i.write(writer)?;
                        j.write(writer)?;
                        writer.write_opcode(OpCode::SetGlobalArray2d);
                        writer.add_inline_operand(*var);
                    }
                },
            },
            Statement::DefFn(lval, fname) => match lval {
                LValue::FnPtr(func) => {
                    writer.write_opcode(OpCode::BindFunc);
                    writer.add_inline_operand(*func);
                    writer.add_inline_operand(
                        writer.func_map.get(*fname).cloned().unwrap(),
                    );
                }
                _ => {}
            },
            Statement::CallSub(fname) => {
                writer.write_opcode(OpCode::Call);
                writer.add_inline_operand(
                    writer.func_map.get(*fname).cloned().unwrap(),
                );
                writer.write(0); // n args
            }
            Statement::Print(expr) => {
                expr.write(writer)?;
                writer.write_opcode(OpCode::PrintExpr);
            }
            Statement::PrintLabel(offset, len) => {
                writer.write_opcode(OpCode::PrintLabel);
                writer.add_operand(
                    writer.get_string_label(*offset, *len).to_owned(),
                );
            }
            Statement::PrintAdvance15 => {
                writer.write_opcode(OpCode::PrintAdvance15);
            }
            Statement::PrintAdvance3 => {
                writer.write_opcode(OpCode::PrintAdvance3);
            }
            Statement::PrintNewline => {
                writer.write_opcode(OpCode::PrintNewline);
            }
            Statement::Alloc1d(lval, expr) => match lval {
                LValue::ArrPtr(var, _) => {
                    expr.write(writer)?;
                    writer.write_opcode(OpCode::DefineDim1d);
                    writer.add_inline_operand(*var);
                }
                _ => {}
            },
            Statement::Alloc2d(lval, m, n) => match lval {
                LValue::ArrPtr(var, _) => {
                    m.write(writer)?;
                    n.write(writer)?;
                    writer.write_opcode(OpCode::DefineDim2d);
                    writer.add_inline_operand(*var);
                }
                _ => {}
            },
        }

        Ok(())
    }
}

impl ChunkWrite for Expr {
    fn write(&self, writer: &mut ChunkWriter) -> Result<(), WriteError> {
        match self {
            Expr::ReadData => {
                writer.write_opcode(OpCode::Read);
            }
            Expr::Const(n) => {
                writer.write_opcode(OpCode::Constant);
                writer.add_operand(*n);
            }
            Expr::RandF64 => {
                writer.write_opcode(OpCode::Rand);
            }
            Expr::Get(lval) => match lval.deref() {
                LValue::Global(var) => {
                    writer.write_opcode(OpCode::GetGlobal);
                    writer.add_inline_operand(*var);
                }
                LValue::Local(index) => {
                    writer.write_opcode(OpCode::GetLocal);
                    writer.add_inline_operand(LocalVar::from(*index));
                }
                LValue::FnPtr(func) => {
                    writer.write_opcode(OpCode::GetFunc);
                    writer.add_inline_operand(*func);
                }
                LValue::ArrPtr(var, offset) => match offset {
                    Offset::OneD(i) => {
                        i.write(writer)?;
                        writer.write_opcode(OpCode::GetGlobalArray1d);
                        writer.add_inline_operand(*var);
                    }
                    Offset::TwoD(i, j) => {
                        i.write(writer)?;
                        j.write(writer)?;
                        writer.write_opcode(OpCode::GetGlobalArray2d);
                        writer.add_inline_operand(*var);
                    }
                },
            },
            Expr::Call(lval, expr) => match lval.deref() {
                LValue::FnPtr(func) => {
                    expr.write(writer)?;
                    writer.write_opcode(OpCode::GetFunc);
                    writer.add_inline_operand(*func);
                    writer.write_opcode(OpCode::CallIndirect);
                    writer.write(1); // 1 arg
                }
                _ => return Err(WriteError("Type error")),
            },
            Expr::Unary(op, expr) => {
                expr.write(writer)?;
                match op {
                    UnaryOp::Not => {
                        writer.write_opcode(OpCode::Not);
                    }
                    UnaryOp::Neg => {
                        writer.write_opcode(OpCode::Negate);
                    }
                    UnaryOp::Sin => {
                        writer.write_opcode(OpCode::CallNative);
                        writer.add_inline_operand(Func::Sin);
                    }
                    UnaryOp::Cos => {
                        writer.write_opcode(OpCode::CallNative);
                        writer.add_inline_operand(Func::Cos);
                    }
                    UnaryOp::Atn => {
                        writer.write_opcode(OpCode::CallNative);
                        writer.add_inline_operand(Func::Atn);
                    }
                    UnaryOp::Exp => {
                        writer.write_opcode(OpCode::CallNative);
                        writer.add_inline_operand(Func::Exp);
                    }
                    UnaryOp::Abs => {
                        writer.write_opcode(OpCode::CallNative);
                        writer.add_inline_operand(Func::Abs);
                    }
                    UnaryOp::Log => {
                        writer.write_opcode(OpCode::CallNative);
                        writer.add_inline_operand(Func::Log);
                    }
                    UnaryOp::Sqr => {
                        writer.write_opcode(OpCode::CallNative);
                        writer.add_inline_operand(Func::Sqr);
                    }
                    UnaryOp::Trunc => {
                        writer.write_opcode(OpCode::CallNative);
                        writer.add_inline_operand(Func::Int);
                    }
                }
            }
            Expr::Binary(op, lhs, rhs) => {
                lhs.write(writer)?;
                rhs.write(writer)?;
                match op {
                    BinaryOp::Add => {
                        writer.write_opcode(OpCode::Add);
                    }
                    BinaryOp::Sub => {
                        writer.write_opcode(OpCode::Sub);
                    }
                    BinaryOp::Mul => {
                        writer.write_opcode(OpCode::Mul);
                    }
                    BinaryOp::Div => {
                        writer.write_opcode(OpCode::Div);
                    }
                    BinaryOp::Pow => {
                        writer.write_opcode(OpCode::Pow);
                    }
                    BinaryOp::Less => {
                        writer.write_opcode(OpCode::Less);
                    }
                    BinaryOp::Greater => {
                        writer.write_opcode(OpCode::Greater);
                    }
                    BinaryOp::Equal => {
                        writer.write_opcode(OpCode::Equal);
                    }
                    BinaryOp::CopySign => {
                        writer.write_opcode(OpCode::CopySign);
                    }
                }
            }
        }
        Ok(())
    }
}
