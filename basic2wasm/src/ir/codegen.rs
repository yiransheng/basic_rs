use std::fmt::Display;

use super::runtime::runtime_api;
use super::{
    BasicBlock, BinaryOp as IRBinaryOp, BlockExit, Expr as IRExpr, Function,
    FunctionName, GlobalKind, LValue, Label, Offset, Program, Statement,
    UnaryOp as IRUnaryOp,
};

use binaryen::*;
use byteorder::{ByteOrder, LittleEndian, WriteBytesExt};
use slotmap::SecondaryMap;

static MODULE_BASE: &'static [u8] = include_bytes!("../runtime.wasm");

const ALLOC1D_INDEX: u32 = 0;
const ALLOC2D_INDEX: u32 = 1;
const LOAD1D_INDEX: u32 = 2;
const LOAD2D_INDEX: u32 = 3;
const STORE1D_INDEX: u32 = 4;
const STORE2D_INDEX: u32 = 5;

const F64_SIZE: usize = 8;

fn array_memory_start(ir: &Program) -> u32 {
    let data_end = ir.data.len() * F64_SIZE;
    let labels_end = data_end + ir.labels.as_bytes().len();
    // alignment: 8 bytes
    let arr_start = (labels_end | 7) + 1;

    arr_start as u32
}

fn array_name<V: Display>(var: V) -> String {
    format!("array_{}", var)
}

pub struct CodeGen {
    module: Module,
    ir: Program,
    func_names: SecondaryMap<FunctionName, String>,
    main_type: FnType,
}

impl CodeGen {
    pub fn new(ir: Program) -> Self {
        let module = Module::read(MODULE_BASE);
        let main_type = module.add_fn_type(Some("main"), &[], Ty::None);
        let mut func_names = SecondaryMap::new();

        for (i, function) in ir.functions.iter().enumerate() {
            if function.name == ir.main {
                func_names.insert(function.name, String::from("main"));
            } else {
                func_names.insert(function.name, format!("fn${}", i));
            }
        }

        CodeGen {
            module,
            ir,
            func_names,
            main_type,
        }
    }
    pub fn generate(mut self) -> Module {
        let print_api =
            self.module
                .add_fn_type(None::<&str>, &[ValueTy::F64], Ty::None);
        let print_label_api = self.module.add_fn_type(
            None::<&str>,
            &[ValueTy::I32, ValueTy::I32],
            Ty::None,
        );
        let print_control_api =
            self.module.add_fn_type(None::<&str>, &[], Ty::None);
        let rand_api = self.module.add_fn_type(None::<&str>, &[], Ty::F64);
        let pow_api = self.module.add_fn_type(
            None::<&str>,
            &[ValueTy::F64, ValueTy::F64],
            Ty::F64,
        );
        self.module
            .add_fn_import("print", "env", "print", &print_api);
        self.module.add_fn_import(
            "printLabel",
            "env",
            "printLabel",
            &print_label_api,
        );
        self.module.add_fn_import(
            "printNewline",
            "env",
            "printNewline",
            &print_control_api,
        );
        self.module.add_fn_import(
            "printAdvance3",
            "env",
            "printAdvance3",
            &print_control_api,
        );
        self.module.add_fn_import(
            "printAdvance15",
            "env",
            "printAdvance15",
            &print_control_api,
        );

        self.module.add_fn_import("rand", "env", "rand", &rand_api);
        self.module.add_fn_import("pow", "env", "pow", &pow_api);

        self.gen_data();

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
                GlobalKind::ArrPtr(var) => {
                    self.module.add_global(
                        array_name(var),
                        ValueTy::I32,
                        true,
                        // TODO: init to nullptr
                        self.module.const_(Literal::I32(0)),
                    );
                }
                _ => unimplemented!(),
            }
        }

        runtime_api(&self.module);

        let _ = self.module.add_fn_type(
            Some("alloc2d"),
            // ptr, nrow, ncol
            &[ValueTy::I32, ValueTy::I32, ValueTy::I32],
            Ty::I32, // ptr
        );

        let _ = self.module.add_fn_type(
            Some("load1d"),
            // ptr, index
            &[ValueTy::I32, ValueTy::I32],
            Ty::F64,
        );
        let _ = self.module.add_fn_type(
            Some("load2d"),
            // ptr, row, col
            &[ValueTy::I32, ValueTy::I32, ValueTy::I32],
            Ty::F64,
        );
        let _ = self.module.add_fn_type(
            Some("store1d"),
            // ptr, index
            &[ValueTy::I32, ValueTy::I32, ValueTy::F64],
            Ty::None,
        );
        let _ = self.module.add_fn_type(
            Some("store2d"),
            // ptr, row, col
            &[ValueTy::I32, ValueTy::I32, ValueTy::I32, ValueTy::F64],
            Ty::None,
        );

        let functions = ::std::mem::replace(&mut self.ir.functions, vec![]);

        for function in &functions {
            let name = self.func_names.get(function.name).cloned().unwrap();
            self.gen_function(&name, function);
        }

        self.module.add_fn_export("main", "main");

        self.module
    }

    fn gen_data(&mut self) {
        let data_len = self.ir.data.len();
        let data_end = data_len * F64_SIZE;
        let init_expr = self.module.const_(Literal::I32(data_end as u32));
        self.module
            .add_global("data_end", ValueTy::I32, true, init_expr);

        // data segment + string labels
        let mut segments = Vec::with_capacity(2);
        let mut data_bytes: Vec<u8> = Vec::with_capacity(data_end);
        for d in &self.ir.data {
            data_bytes.write_f64::<LittleEndian>(*d).unwrap();
        }

        let offset_expr = self.module.const_(Literal::I32(0));
        let data_segment = Segment::new(&data_bytes, offset_expr);
        segments.push(data_segment);

        let labels_offset_expr =
            self.module.const_(Literal::I32(data_end as u32));
        let labels_segement =
            Segment::new(&self.ir.labels.as_bytes(), labels_offset_expr);
        segments.push(labels_segement);

        self.module.set_memory(1, 1, Some("data"), segments);

        let data_end_ptr = self.module.get_global("data_end", ValueTy::I32);
        let load = self.module.load(
            F64_SIZE as u32,
            true,
            0,
            8,
            ValueTy::F64,
            self.module.get_global("data_end", ValueTy::I32),
        );

        let decr = self.module.set_global(
            "data_end",
            self.module.binary(
                BinaryOp::SubI32,
                self.module.get_global("data_end", ValueTy::I32),
                self.module.const_(Literal::I32(F64_SIZE as u32)),
            ),
        );

        let body = self.module.if_(
            data_end_ptr,
            self.module.block::<&'static str, _>(
                None,
                vec![decr, self.module.return_(Some(load))],
                None,
            ),
            Some(self.module.unreachable()),
        );
        let read_type = self.module.add_fn_type(Some("read"), &[], Ty::F64);
        self.module.add_fn("read", &read_type, &[], body);
    }

    fn gen_main_entry(&mut self) -> Expr {
        let arr_start = array_memory_start(&self.ir);
        let arr_ptr = self
            .module
            .const_(Literal::I32(arr_start + (F64_SIZE as u32)));
        let init_arr_ptr = self.module.store(
            4, // pointer size
            0, // offset
            4, // align
            self.module.const_(Literal::I32(arr_start)),
            arr_ptr,
            ValueTy::I32,
        );

        let data_len = self.ir.data.len();
        let data_end = data_len * F64_SIZE;

        let reset_data = self.module.set_global(
            "data_end",
            self.module.const_(Literal::I32(data_end as u32)),
        );

        self.module.block::<&'static str, _>(
            None,
            vec![reset_data, init_arr_ptr],
            None,
        )
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

        let mut entry_block = plain_blocks.get(function.entry).unwrap().clone();

        if name == "main" {
            let main_entry = relooper.add_block(self.gen_main_entry());
            relooper.add_branch(main_entry, entry_block, None, None);
            entry_block = main_entry;
        }

        let body = relooper.render(entry_block, 0);

        let locals = vec![ValueTy::F64; function.local_count];
        self.module.add_fn(name, &self.main_type, &locals, body);
    }
    fn gen_block(&self, block: &BasicBlock) -> Expr {
        for statement in &block.statements {
            let _expr = self.statement(statement);
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
            IRExpr::RandF64 => self.module.call("rand", None, Ty::F64),
            IRExpr::ReadData => self.module.call("read", None, Ty::F64),
            IRExpr::Const(v) => self.module.const_(Literal::F64(*v)),
            IRExpr::Unary(op, rhs) => {
                let rhs = self.expr(rhs);
                self.module.unary((*op).into(), rhs)
            }
            IRExpr::Binary(op, lhs, rhs) => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                match op {
                    IRBinaryOp::Pow => {
                        self.module.call("pow", vec![lhs, rhs], Ty::F64)
                    }
                    _ => self.module.binary((*op).into(), lhs, rhs),
                }
            }
            IRExpr::Get(lval) => match lval.deref() {
                LValue::Global(var) => {
                    self.module.get_global(var.to_string(), ValueTy::F64)
                }
                LValue::Local(index) => {
                    self.module.get_local(*index as u32, ValueTy::F64)
                }
                LValue::ArrPtr(var, offset) => {
                    let ptr =
                        self.module.get_global(array_name(var), ValueTy::I32);
                    match offset {
                        Offset::OneD(index) => {
                            let index = self.module.unary(
                                UnaryOp::TruncUF64ToI32,
                                self.expr(index),
                            );
                            self.module.call_indirect(
                                self.module.const_(Literal::I32(LOAD1D_INDEX)),
                                vec![ptr, index],
                                "load1d",
                            )
                        }
                        Offset::TwoD(i, j) => {
                            let i = self
                                .module
                                .unary(UnaryOp::TruncUF64ToI32, self.expr(i));
                            let j = self
                                .module
                                .unary(UnaryOp::TruncUF64ToI32, self.expr(j));

                            self.module.call_indirect(
                                self.module.const_(Literal::I32(LOAD2D_INDEX)),
                                vec![ptr, i, j],
                                "load2d",
                            )
                        }
                    }
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
                LValue::ArrPtr(var, offset) => {
                    let ptr =
                        self.module.get_global(array_name(var), ValueTy::I32);
                    match offset {
                        Offset::OneD(index) => {
                            let index = self.module.unary(
                                UnaryOp::TruncUF64ToI32,
                                self.expr(index),
                            );
                            self.module.call(
                                "store1d_",
                                vec![ptr, index, self.expr(expr)],
                                Ty::None,
                            )
                        }
                        Offset::TwoD(i, j) => {
                            let i = self
                                .module
                                .unary(UnaryOp::TruncUF64ToI32, self.expr(i));
                            let j = self
                                .module
                                .unary(UnaryOp::TruncUF64ToI32, self.expr(j));

                            self.module.call_indirect(
                                self.module.const_(Literal::I32(STORE2D_INDEX)),
                                vec![ptr, i, j, self.expr(expr)],
                                "store2d",
                            )
                        }
                    }
                }
                _ => unimplemented!(),
            },
            Statement::Alloc1d(lval, size) => {
                let arr = match lval {
                    LValue::ArrPtr(var, _) => array_name(var),
                    _ => panic!("type error"),
                };
                let arr_start = array_memory_start(&self.ir);
                let arr_start = self.module.const_(Literal::I32(arr_start));
                let size =
                    self.module.unary(UnaryOp::TruncUF64ToI32, self.expr(size));

                let ptr = self.module.call(
                    "alloc1d_",
                    vec![arr_start, size],
                    Ty::I32,
                );

                self.module.set_global(arr, ptr)
            }
            Statement::Alloc2d(lval, nrow, ncol) => {
                let arr = match lval {
                    LValue::ArrPtr(var, _) => array_name(var),
                    _ => panic!("type error"),
                };
                let arr_start = array_memory_start(&self.ir);
                let arr_start = self.module.const_(Literal::I32(arr_start));
                let nrow =
                    self.module.unary(UnaryOp::TruncUF64ToI32, self.expr(nrow));
                let ncol =
                    self.module.unary(UnaryOp::TruncUF64ToI32, self.expr(ncol));
                let ptr = self.module.call_indirect(
                    self.module.const_(Literal::I32(ALLOC2D_INDEX)),
                    vec![arr_start, nrow, ncol],
                    "alloc2d",
                );

                self.module.set_global(arr, ptr)
            }

            Statement::Print(expr) => {
                self.module.call("print", Some(self.expr(expr)), Ty::None)
            }
            Statement::PrintAdvance3 => {
                self.module.call("printAdvance3", None, Ty::None)
            }
            Statement::PrintAdvance15 => {
                self.module.call("printAdvance15", None, Ty::None)
            }
            Statement::PrintNewline => {
                self.module.call("printNewline", None, Ty::None)
            }
            Statement::PrintLabel(offset, length) => {
                let data_len = self.ir.data.len();
                let label_start = data_len * F64_SIZE + offset;

                let offset =
                    self.module.const_(Literal::I32(label_start as u32));
                let length = self.module.const_(Literal::I32(*length as u32));

                self.module
                    .call("printLabel", vec![offset, length], Ty::None)
            }
            Statement::CallSub(name) => {
                let name: &str = &*self.func_names.get(*name).unwrap();
                self.module.call(name, None, Ty::None)
            }
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
