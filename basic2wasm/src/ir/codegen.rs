use super::{
    BinaryOp as IRBinaryOp, Branches, Expression, JumpKind, Label, Statement,
    SymbolKind, UnaryOp as IRUnaryOp, IR,
};
use binaryen::*;
use rustc_hash::FxHashMap;

// static MODULE_BASE: &'static [u8] = include_bytes!("../runtime.wasm");

const ALLOC1d_INDEX: u32 = 1;
const ALLOC2d_INDEX: u32 = 2;
const LOAD1d_INDEX: u32 = 3;
const LOAD2d_INDEX: u32 = 4;
const STORE1d_INDEX: u32 = 5;
const STORE2d_INDEX: u32 = 6;

pub struct CodeGen {
    module: Module,
    relooper: Relooper,
    ir: IR,
    blocks: FxHashMap<Label, PlainBlock>,
}

impl CodeGen {
    pub fn new(ir: IR) -> Self {
        let module = Module::new();
        let relooper = module.relooper();

        CodeGen {
            module,
            relooper,
            ir,
            blocks: FxHashMap::default(),
        }
    }
    pub fn generate(mut self) -> Module {
        let labels: Vec<_> = self.ir.blocks.keys().collect();

        let mut local_count = 0; // 0 as logical register
        for sym_kind in self.ir.symbols.values() {
            match sym_kind {
                SymbolKind::Global(var) => {
                    let init_expr = self.module.const_(Literal::F64(0.0));

                    self.module.add_global(
                        var.to_string(),
                        ValueTy::F64,
                        true,
                        init_expr,
                    );
                }
                SymbolKind::Local(index) => {
                    if *index > local_count {
                        local_count = *index;
                    }
                }
            }
        }

        for label in &labels {
            self.block(*label);
        }
        for label in &labels {
            self.branch(*label);
        }

        let print_api =
            self.module
                .add_fn_type(None::<&str>, &[ValueTy::F64], Ty::None);
        let rand_api = self.module.add_fn_type(None::<&str>, &[], Ty::F64);

        self.module
            .add_fn_import("print", "env", "print", &print_api);

        self.module.add_fn_import("rand", "env", "rand", &rand_api);

        let entry_id = self.blocks.get(&self.ir.entry_block).unwrap();
        let body = self.relooper.render(entry_id.clone(), 0);

        let locals = vec![ValueTy::F64; local_count + 1];

        let main_ty = self.module.add_fn_type(Some("main"), &[], Ty::None);

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

        let _main = self.module.add_fn("main", &main_ty, &locals, body);

        self.module.add_fn_export("main", "main");

        self.module
    }

    fn branch(&mut self, label: Label) {
        let from_block = *self.blocks.get(&label).unwrap();

        self.ir
            .branches
            .get(label)
            .into_iter()
            .flat_map(Branches::iter)
            .map(|&(jump_kind, to)| (jump_kind, self.blocks.get(&to).unwrap()))
            .for_each(|(jump_kind, to_block)| {
                let cond_expr = match self
                    .ir
                    .code
                    .get(label)
                    .into_iter()
                    .flat_map(|stmts| stmts.iter())
                    .last()
                {
                    Some(Statement::Logical(ref expr)) => Some(self.expr(expr)),
                    _ => None,
                };
                let cond = match jump_kind {
                    JumpKind::Jmp => None,
                    JumpKind::JmpZ => cond_expr
                        .map(|expr| self.module.unary(UnaryOp::EqZI32, expr)),
                    JumpKind::JmpNZ => cond_expr,
                };

                self.relooper.add_branch(
                    from_block.clone(),
                    to_block.clone(),
                    cond,
                    None,
                );
            });
    }
    fn block(&mut self, label: Label) {
        let exprs = self
            .ir
            .code
            .get(label)
            .unwrap()
            .iter()
            .map(|stmt| self.statement(stmt));

        let block_expr =
            self.module.block::<&'static str, _>(None, exprs, None);

        let block_id = self.relooper.add_block(block_expr);

        self.blocks.insert(label, block_id);
    }

    fn expr(&self, expr: &Expression) -> Expr {
        match expr {
            Expression::Const(v) => self.module.const_(Literal::F64(*v)),
            Expression::Unary(op, rhs) => {
                let rhs = self.expr(rhs);

                self.module.unary((*op).into(), rhs)
            }
            Expression::Binary(op, lhs, rhs) => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                self.module.binary((*op).into(), lhs, rhs)
            }
            Expression::Get(sym) => match self.ir.symbol_kind(*sym) {
                SymbolKind::Global(var) => {
                    self.module.get_global(var.to_string(), ValueTy::F64)
                }
                SymbolKind::Local(index) => {
                    self.module.get_local(index as u32, ValueTy::F64)
                }
            },
            Expression::Load(sym, index) => {
                let index = self.expr(index);
                let index = self.module.unary(UnaryOp::TruncUF64ToI32, index);

                let sym_kind = self.ir.symbols.get(*sym).unwrap();
                let ptr = match sym_kind {
                    SymbolKind::Local(index) => {
                        self.module.get_local(*index as u32, ValueTy::I32)
                    }
                    _ => panic!("bad symbol"),
                };

                self.module.call_indirect(
                    self.module.const_(Literal::I32(LOAD1d_INDEX)),
                    vec![ptr, index],
                    "i32_i32_to_i32",
                )
            }
            Expression::Load2d(..) => unimplemented!(),
            Expression::LoopCondition(exprs) => {
                let step = self.expr(&exprs[0]);
                let target = self.expr(&exprs[1]);
                let current = self.expr(&exprs[2]);

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

                done
            }
        }
    }
    fn statement(&self, stmt: &Statement) -> Expr {
        match stmt {
            Statement::Assign(sym, expr) => match self.ir.symbol_kind(*sym) {
                SymbolKind::Global(var) => {
                    self.module.set_global(var.to_string(), self.expr(expr))
                }
                SymbolKind::Local(index) => {
                    self.module.set_local(index as u32, self.expr(expr))
                }
            },
            Statement::Logical(_) => self.module.nop(),
            Statement::Print(expr) => {
                self.module.call("print", Some(self.expr(expr)), Ty::None)
            }
            Statement::Store(..) => unimplemented!(),
            Statement::Store2d(..) => unimplemented!(),
            Statement::Alloc(sym, size) => {
                let size = self.expr(size);
                let size = self.module.unary(UnaryOp::TruncUF64ToI32, size);

                let ptr = self.module.call_indirect(
                    self.module.const_(Literal::I32(ALLOC1d_INDEX)),
                    vec![size],
                    "i32_to_i32",
                );
                let sym_kind = self.ir.symbols.get(*sym).unwrap();
                match sym_kind {
                    SymbolKind::Local(index) => {
                        self.module.set_local(*index as u32, ptr)
                    }
                    _ => panic!("bad symbol"),
                }
            }
            Statement::Alloc2d(sym, row_size, col_size) => {
                let row_size = self.expr(row_size);
                let row_size =
                    self.module.unary(UnaryOp::TruncUF64ToI32, row_size);

                let col_size = self.expr(col_size);
                let col_size =
                    self.module.unary(UnaryOp::TruncUF64ToI32, col_size);

                let ptr = self.module.call_indirect(
                    self.module.const_(Literal::I32(ALLOC2d_INDEX)),
                    vec![row_size, col_size],
                    "i32_i32_to_i32",
                );

                let sym_kind = self.ir.symbols.get(*sym).unwrap();
                match sym_kind {
                    SymbolKind::Local(index) => {
                        self.module.set_local(*index as u32, ptr)
                    }
                    _ => panic!("bad symbol"),
                }
            }
        }
    }
}

impl Into<UnaryOp> for IRUnaryOp {
    fn into(self) -> UnaryOp {
        match self {
            IRUnaryOp::Neg => UnaryOp::NegF64,
            IRUnaryOp::EqZ => UnaryOp::EqZI32,
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
