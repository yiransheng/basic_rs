use std::collections::VecDeque;

use binaryen::*;
use either::Either;
use rustc_hash::FxHashMap;

use basic_rs::{
    CompileErrorInner, Func, IRVisitor, Instruction as Instr, InstructionKind,
    Label, Variable,
};

type StackItem = Either<Label, Expr>;

pub enum WasmError {
    CompileError,
    Unsupported,
}

impl Into<CompileErrorInner> for WasmError {
    fn into(self) -> CompileErrorInner {
        CompileErrorInner::Custom("Wasm error")
    }
}

pub struct WasmModule {
    module: Module,
    entry_id: Option<PlainBlock>,
    relooper: Relooper,
    exprs: VecDeque<StackItem>,
    blocks: FxHashMap<Label, PlainBlock>,
    jumps: Vec<(PlainBlock, Label, Option<Expr>)>,
    prev_instr: InstructionKind,
    prev_block_id: Option<PlainBlock>,
}

impl Default for WasmModule {
    fn default() -> Self {
        let module = Module::new();
        let relooper = module.relooper();

        WasmModule {
            module: module,
            entry_id: None,
            relooper,
            exprs: VecDeque::new(),
            blocks: FxHashMap::default(),
            jumps: vec![],
            prev_instr: InstructionKind::Noop,
            prev_block_id: None,
        }
    }
}

impl WasmModule {
    fn constant(&mut self, v: f64) {
        let value = self.module.const_(Literal::F64(v));
        self.exprs.push_back(Either::Right(value));
    }
    fn define_global(&mut self, var: Variable) {
        let init_expr = self.module.const_(Literal::F64(0.0));
        self.module.add_global(
            var.to_string(),
            ValueTy::F64,
            /* mutable = */ true,
            init_expr,
        );
    }
    fn pop_expr(&mut self) -> Result<Expr, WasmError> {
        match self.exprs.pop_back() {
            Some(Either::Right(expr)) => Ok(expr),
            _ => Err(WasmError::CompileError),
        }
    }
    fn push_expr(&mut self, expr: Expr) {
        self.exprs.push_back(Either::Right(expr));
    }

    fn pop_block(&mut self) -> Result<PlainBlock, WasmError> {
        let mut exprs = vec![];
        while let Some(Either::Right(_)) = self.exprs.back() {
            if let Some(Either::Right(expr)) = self.exprs.pop_back() {
                exprs.push(expr);
            }
        }
        exprs.reverse();

        let block_expr =
            self.module.block::<&'static str, _>(None, exprs, None);

        let block_id = self.relooper.add_block(block_expr);

        if let Some(Either::Left(ref label)) = self.exprs.back() {
            self.blocks.insert(*label, block_id);
            self.exprs.pop_back();
        }

        if self.entry_id.is_none() {
            self.entry_id = Some(block_id);
        }

        self.prev_block_id = Some(block_id);

        Ok(block_id)
    }
}

impl IRVisitor for WasmModule {
    type Output = Module;
    type Error = WasmError;

    fn finish(mut self) -> Result<Self::Output, Self::Error> {
        let print_api =
            self.module
                .add_fn_type(None::<&str>, &[ValueTy::F64], Ty::None);
        let rand_api = self.module.add_fn_type(None::<&str>, &[], Ty::F64);

        self.module
            .add_fn_import("print", "env", "print", &print_api);

        self.module.add_fn_import("rand", "env", "rand", &rand_api);

        let entry_id = match self.entry_id {
            Some(id) => id,
            None => self.pop_block()?,
        };
        for (from_block, to_label, cond) in self.jumps.drain(..) {
            let to_block = self
                .blocks
                .get(&to_label)
                .ok_or_else(|| WasmError::CompileError)?;
            self.relooper
                .add_branch(from_block, to_block.clone(), cond, None);
        }

        let body = self.relooper.render(entry_id, 0);

        let params = &[];
        let main_ty = self.module.add_fn_type(Some("main"), params, Ty::None);
        let _main = self.module.add_fn("main", &main_ty, &[], body);

        self.module.add_fn_export("main", "main");

        Ok(self.module)
    }

    fn visit_instruction(&mut self, instr: Instr) -> Result<(), Self::Error> {
        println!("Inst: {:?}", instr);
        let Instr { kind, label, .. } = instr;

        if let Some(label) = label {
            match self.prev_instr {
                InstructionKind::Jump(_) => {}
                InstructionKind::JumpTrue(_) => {
                    let prev_block = self.prev_block_id.unwrap();
                    self.jumps.push((prev_block, label, None));
                }
                InstructionKind::JumpFalse(_) => {
                    let prev_block = self.prev_block_id.unwrap();
                    self.jumps.push((prev_block, label, None));
                }
                _ => {
                    let prev_block = self.pop_block()?;
                    self.jumps.push((prev_block, label, None));
                }
            }

            self.exprs.push_back(Either::Left(label));
        }

        match kind {
            InstructionKind::Noop => {
                let nop = self.module.nop();
                self.push_expr(nop);
            }
            InstructionKind::DefineGlobal(var) => {
                self.define_global(var);
            }
            InstructionKind::Constant(n) => {
                self.constant(n);
            }
            InstructionKind::GetGlobal(var) => {
                let expr =
                    self.module.get_global(var.to_string(), ValueTy::F64);
                self.push_expr(expr);
            }
            InstructionKind::SetGlobal(var) => {
                let expr = self.pop_expr()?;
                let set_op = self.module.set_global(var.to_string(), expr);
                self.push_expr(set_op);
            }
            InstructionKind::Dup => {
                let expr = self.pop_expr()?;
                let expr = self.module.tee_local(0, expr);
                self.push_expr(expr);
                let expr = self.module.get_local(0, ValueTy::F64);
                self.push_expr(expr);
            }
            // f64 binary ops
            InstructionKind::Add => {
                let rhs = self.pop_expr()?;
                let lhs = self.pop_expr()?;
                let sum = self.module.binary(BinaryOp::AddF64, lhs, rhs);
                self.push_expr(sum);
            }
            InstructionKind::Sub => {
                let rhs = self.pop_expr()?;
                let lhs = self.pop_expr()?;
                let v = self.module.binary(BinaryOp::SubF64, lhs, rhs);
                self.push_expr(v);
            }
            InstructionKind::Mul => {
                let rhs = self.pop_expr()?;
                let lhs = self.pop_expr()?;
                let v = self.module.binary(BinaryOp::MulF64, lhs, rhs);
                self.push_expr(v);
            }
            InstructionKind::Div => {
                let rhs = self.pop_expr()?;
                let lhs = self.pop_expr()?;
                let v = self.module.binary(BinaryOp::DivF64, lhs, rhs);
                self.push_expr(v);
            }
            InstructionKind::Equal => {
                let rhs = self.pop_expr()?;
                let lhs = self.pop_expr()?;
                let r = self.module.binary(BinaryOp::EqF64, lhs, rhs);
                self.push_expr(r);
            }
            InstructionKind::Less => {
                let rhs = self.pop_expr()?;
                let lhs = self.pop_expr()?;
                let r = self.module.binary(BinaryOp::LtF64, lhs, rhs);
                self.push_expr(r);
            }
            InstructionKind::Greater => {
                let rhs = self.pop_expr()?;
                let lhs = self.pop_expr()?;
                let r = self.module.binary(BinaryOp::GtF64, lhs, rhs);
                self.push_expr(r);
            }
            // f64 unary ops
            InstructionKind::Negate => {
                let value = self.pop_expr()?;
                let value = self.module.unary(UnaryOp::NegF64, value);
                self.push_expr(value);
            }
            // boolean
            InstructionKind::Not => {
                let value = self.pop_expr()?;
                let value = self.module.unary(UnaryOp::EqZI32, value);
                self.push_expr(value);
            }
            // native fns
            InstructionKind::CallNative(func) => match func {
                Func::Rnd => {
                    let value = self.module.call("rand", None, Ty::F64);
                    self.push_expr(value);
                }
                _ => unimplemented!(),
            },
            // control flow
            InstructionKind::Jump(to_label) => {
                let from_block = self.pop_block()?;
                self.jumps.push((from_block, to_label, None));
            }
            InstructionKind::JumpTrue(to_label) => {
                let cond = self.pop_expr()?;
                let from_block = self.pop_block()?;
                self.jumps.push((from_block, to_label, Some(cond)));
            }
            InstructionKind::JumpFalse(to_label) => {
                let cond = self.pop_expr()?;
                let cond = self.module.unary(UnaryOp::EqZI32, cond);
                let from_block = self.pop_block()?;
                self.jumps.push((from_block, to_label, Some(cond)));
            }

            InstructionKind::LoopTest => {
                const STEP_INDEX: u32 = 1;
                const TARGET_INDEX: u32 = 2;

                let current = self.pop_expr()?;
                let step = self.pop_expr()?;
                let target = self.pop_expr()?;

                let step = self.module.tee_local(STEP_INDEX, step);
                let target = self.module.tee_local(TARGET_INDEX, target);

                let dir = self.module.binary(
                    BinaryOp::CopySignF64,
                    self.module.const_(Literal::F64(1.0)),
                    step,
                );
                let dist =
                    self.module.binary(BinaryOp::SubF64, current, target);
                let dist = self.module.binary(BinaryOp::MulF64, dist, dir);
                let zero = self.module.const_(Literal::F64(0.0));
                let done = self.module.binary(BinaryOp::GtF64, dist, zero);

                let restore_ops = vec![
                    self.module.get_local(TARGET_INDEX, ValueTy::F64),
                    self.module.get_local(STEP_INDEX, ValueTy::F64),
                    self.module.const_(Literal::I32(0)),
                ];
                let restore_ops = self.module.block::<&'static str, _>(
                    None,
                    restore_ops,
                    None,
                );

                let expr = self.module.if_(
                    done,
                    self.module.const_(Literal::I32(1)),
                    Some(restore_ops),
                );
                self.push_expr(expr);
            }

            InstructionKind::Stop => {
                let ret = self.module.return_(None);
                self.push_expr(ret);
                self.pop_block()?;
            }
            InstructionKind::PrintStart => {}
            InstructionKind::PrintEnd => {}
            InstructionKind::PrintExpr => {
                let value = self.pop_expr()?;
                let expr = self.module.call("print", Some(value), Ty::None);
                self.push_expr(expr);
            }
            _ => unimplemented!(),
        }

        for e in self.exprs.iter() {
            match e {
                Either::Left(label) => println!("Label({})", label),
                Either::Right(expr) => expr.print(),
            }
        }

        self.prev_instr = kind;

        Ok(())
    }
}
