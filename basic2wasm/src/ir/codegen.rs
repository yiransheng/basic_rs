use super::{
    BinaryOp as IRBinaryOp, Branches, Expression, JumpKind, Label, Statement,
    SymbolKind, UnaryOp as IRUnaryOp, IR,
};
use binaryen::*;
use rustc_hash::FxHashMap;

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

        let params = &[];
        let main_ty = self.module.add_fn_type(Some("main"), params, Ty::None);
        let _main = self.module.add_fn("main", &main_ty, &[], body);

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
                let cond = match jump_kind {
                    JumpKind::Jmp => None,
                    JumpKind::JmpZ => Some(self.module.unary(
                        UnaryOp::EqZI32,
                        self.module.get_local(0, ValueTy::I32),
                    )),
                    JumpKind::JmpNZ => {
                        Some(self.module.get_local(0, ValueTy::I32))
                    }
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
                    // local=0 used for conditional
                    // TODO: more explicit
                    self.module.get_local(index as u32 + 1, ValueTy::F64)
                }
            },
            Expression::LoopCondition(_) => unimplemented!(),
        }
    }
    fn statement(&self, stmt: &Statement) -> Expr {
        match stmt {
            Statement::Assign(sym, expr) => match self.ir.symbol_kind(*sym) {
                SymbolKind::Global(var) => {
                    self.module.set_global(var.to_string(), self.expr(expr))
                }
                SymbolKind::Local(index) => {
                    self.module.set_local(index as u32 + 1, self.expr(expr))
                }
            },
            Statement::Logical(expr) => {
                self.module.set_local(0, self.expr(expr))
            }
            Statement::Print(expr) => {
                self.module.call("print", Some(self.expr(expr)), Ty::None)
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
