use super::{
    BasicBlock, BinaryOp as IRBinaryOp, BlockExit, Expr as IRExpr, Function,
    FunctionName, GlobalKind, LValue, Label, Program, Statement,
    UnaryOp as IRUnaryOp,
};

use binaryen::*;
use slotmap::SecondaryMap;

// static MODULE_BASE: &'static [u8] = include_bytes!("../runtime.wasm");

const ALLOC1d_INDEX: u32 = 1;
const ALLOC2d_INDEX: u32 = 2;
const LOAD1d_INDEX: u32 = 3;
const LOAD2d_INDEX: u32 = 4;
const STORE1d_INDEX: u32 = 5;
const STORE2d_INDEX: u32 = 6;

pub struct CodeGen {
    module: Module,
    ir: Program,
    func_names: SecondaryMap<FunctionName, String>,
    main_type: FnType,
}

impl CodeGen {
    pub fn new(ir: Program) -> Self {
        let module = Module::new();
        let main_type = module.add_fn_type(Some("main"), &[], Ty::None);

        CodeGen {
            module,
            ir,
            func_names: SecondaryMap::new(),
            main_type,
        }
    }
    pub fn generate(mut self) -> Module {
        for kind in &self.ir.globals {
            match kind {
                GlobalKind::Variable(var) => {
                    let init_expr = self.module.const_(Literal::F64(0.0));

                    self.module.add_global(
                        var.to_string(),
                        ValueTy::F64,
                        true,
                        init_expr,
                    );
                }
                _ => unimplemented!(),
            }
        }

        let main_name = self.ir.main;

        for (i, function) in self.ir.functions.iter().enumerate() {
            if function.name == main_name {
                self.func_names.insert(function.name, String::from("main"));
            } else {
                self.func_names.insert(function.name, format!("fn${}", i));
            }
        }
        let functions = ::std::mem::replace(&mut self.ir.functions, vec![]);

        for function in &functions {
            let name: &str = &*self.func_names.remove(function.name).unwrap();
            self.gen_function(name, function);
        }

        let print_api =
            self.module
                .add_fn_type(None::<&str>, &[ValueTy::F64], Ty::None);
        let rand_api = self.module.add_fn_type(None::<&str>, &[], Ty::F64);

        self.module
            .add_fn_import("print", "env", "print", &print_api);

        self.module.add_fn_import("rand", "env", "rand", &rand_api);

        let _ = self.module.add_fn_type(
            Some("i32_to_i32"),
            &[ValueTy::I32],
            Ty::I32,
        );
        let _ = self.module.add_fn_type(
            Some("i32_i32_to_i32"),
            &[ValueTy::I32, ValueTy::I32],
            Ty::I32,
        );

        // self.module.add_fn_export("main", "main");

        self.module
    }

    fn gen_function(&mut self, name: &str, function: &Function) {
        // TODO: def function with ty: f64 -> f64

        let mut relooper = self.module.relooper();
        let mut plain_blocks: SecondaryMap<Label, PlainBlock> =
            SecondaryMap::new();

        for (label, block) in function.blocks.iter() {
            let block_expr = self.gen_block(block);
            let plain_block = relooper.add_block(block_expr);
            plain_blocks.insert(label, plain_block);
        }
        for (label, block) in function.blocks.iter() {
            let from_block = plain_blocks.get(label).unwrap().clone();
            match &block.exit {
                BlockExit::Jump(label) => {
                    let to_block = plain_blocks.get(*label).unwrap().clone();
                    relooper.add_branch(from_block, to_block, None, None);
                }
                BlockExit::Switch(cond, true_br, None) => {
                    let to_block = plain_blocks.get(*true_br).unwrap().clone();
                    let cond = self.expr(&cond);
                    relooper.add_branch(from_block, to_block, Some(cond), None);

                    let ret = self.module.return_(None);
                    let ret_block = relooper.add_block(ret);

                    relooper.add_branch(from_block, ret_block, None, None);
                }
                BlockExit::Switch(cond, true_br, Some(false_br)) => {
                    let to_block = plain_blocks.get(*true_br).unwrap().clone();
                    let cond = self.expr(&cond);
                    relooper.add_branch(from_block, to_block, Some(cond), None);
                    let to_block = plain_blocks.get(*false_br).unwrap().clone();
                    relooper.add_branch(from_block, to_block, None, None);
                }
                _ => {}
            };
        }

        let entry_block = plain_blocks.get(function.entry).unwrap().clone();
        let body = relooper.render(entry_block, 0);

        let locals = vec![ValueTy::F64; function.local_count];
        self.module.add_fn(name, &self.main_type, &locals, body);
    }
    fn gen_block(&self, block: &BasicBlock) -> Expr {
        for statement in &block.statements {
            let expr = self.statement(statement);
        }
        let statements = block
            .statements
            .iter()
            .map(|stmt| self.statement(stmt))
            .chain(match &block.exit {
                BlockExit::Return(None) => Some(self.module.return_(None)),
                BlockExit::Return(Some(expr)) => {
                    let expr = self.expr(&expr);
                    Some(self.module.return_(Some(expr)))
                }
                _ => None,
            });

        self.module.block::<&'static str, _>(None, statements, None)
    }

    fn expr(&self, expr: &IRExpr) -> Expr {
        use std::ops::Deref;

        match expr {
            IRExpr::Const(v) => self.module.const_(Literal::F64(*v)),
            IRExpr::RandF64 => self.module.call("rand", None, Ty::F64),
            IRExpr::Unary(op, rhs) => {
                let rhs = self.expr(rhs);

                self.module.unary((*op).into(), rhs)
            }
            IRExpr::Binary(op, lhs, rhs) => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                self.module.binary((*op).into(), lhs, rhs)
            }
            IRExpr::Get(lval) => match lval.deref() {
                LValue::Global(var) => {
                    self.module.get_global(var.to_string(), ValueTy::F64)
                }
                LValue::Local(index) => {
                    self.module.get_local(*index as u32, ValueTy::F64)
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
    fn statement(&self, stmt: &Statement) -> Expr {
        match stmt {
            Statement::Assign(lval, expr) => match lval {
                LValue::Global(var) => {
                    self.module.set_global(var.to_string(), self.expr(expr))
                }
                LValue::Local(index) => {
                    self.module.set_local(*index as u32, self.expr(expr))
                }
                _ => unimplemented!(),
            },
            Statement::Print(expr) => {
                self.module.call("print", Some(self.expr(expr)), Ty::None)
            }
            Statement::CallSub(name) => {
                let name: &str = &*self.func_names.get(*name).unwrap();
                self.module.call(name, None, Ty::None)
            }
            Statement::PrintNewline => self.module.nop(),
            x @ _ => {
                println!("{:?}", x);
                unimplemented!()
            }
        }
    }
}

impl Into<UnaryOp> for IRUnaryOp {
    fn into(self) -> UnaryOp {
        match self {
            IRUnaryOp::Neg => UnaryOp::NegF64,
            IRUnaryOp::Not => UnaryOp::EqZI32,
            IRUnaryOp::Abs => UnaryOp::AbsF64,
            IRUnaryOp::Sqr => UnaryOp::SqrtF64,
            IRUnaryOp::Trunc => UnaryOp::TruncF64,
            _ => panic!("unsupported op"),
        }
    }
}

impl Into<BinaryOp> for IRBinaryOp {
    fn into(self) -> BinaryOp {
        match self {
            IRBinaryOp::Add => BinaryOp::AddF64,
            IRBinaryOp::Sub => BinaryOp::SubF64,
            IRBinaryOp::Mul => BinaryOp::MulF64,
            IRBinaryOp::Div => BinaryOp::DivF64,
            IRBinaryOp::Less => BinaryOp::LtF64,
            IRBinaryOp::Greater => BinaryOp::GtF64,
            IRBinaryOp::Equal => BinaryOp::EqF64,
            IRBinaryOp::CopySign => BinaryOp::CopySignF64,
            _ => panic!("unsupported op"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::builder::IRBuilder;
    use super::*;

    #[test]
    fn test_gen() {
        let mut builder = IRBuilder::new();
        let block1 = builder.create_block();
        let cond = Expression::Binary(
            IRBinaryOp::Less,
            Box::new(Expression::Const(0.0)),
            Box::new(Expression::Const(1.0)),
        );
        builder.add_statement(block1, Statement::Logical(cond));

        let block2 = builder.create_block();
        let block3 = builder.create_block();
        builder.add_branch(JumpKind::JmpNZ, block1, block2);
        builder.add_branch(JumpKind::Jmp, block1, block3);
        builder.set_entry_block(block1);

        let ir = builder.build();
        let module = CodeGen::new(ir).generate();

        // above step should not panic
        assert!(true);
    }
}
