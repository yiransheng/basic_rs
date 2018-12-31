use binaryen::*;

macro_rules! binaryen_expr {
    ($mod: expr, (escaped $e: expr)) => {
        $e
    };
    ($mod: expr, (i32_const $e: expr)) => {
        $mod.const_(Literal::I32($e))
    };
    ($mod: expr, (i64_const $e: expr)) => {
        $mod.const_(Literal::I64($e))
    };
    ($mod:expr, (i32_get_local $e: expr)) => {
        $mod.get_local($e, ValueTy::I32)
    };
    ($mod:expr, (i32_set_local ($i: expr) ( $($x: tt)* ))) => {{
        let operand = binaryen_expr!($mod, ( $($x)* ));
        $mod.set_local($i, operand)
    }};
    ($mod:expr, (i32_tee_local ($i: expr) ( $($x: tt)* ))) => {{
        let operand = binaryen_expr!($mod, ( $($x)* ));
        $mod.tee_local($i, operand)
    }};
    ($mod:expr, (i32_load (offset $o: expr) ( $($ptr: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        $mod.load(4, true, $o, 4, ValueTy::I32, ptr)
    }};
    ($mod:expr, (i32_store (offset $o: expr) ( $($ptr: tt)* ) (  $($val: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        let val = binaryen_expr!($mod, ( $($val)* ));
        $mod.store(4, $o, 4, ptr, val, ValueTy::I32)
    }};
    ($mod:expr, (i64_load (offset $o: expr) ( $($ptr: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        $mod.load(4, true, $o, 4, ValueTy::I64, ptr)
    }};
    ($mod:expr, (i64_store (offset $o: expr) ( $($ptr: tt)* ) (  $($val: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        let val = binaryen_expr!($mod, ( $($val)* ));
        $mod.store(8, $o, 8, ptr, val, ValueTy::I64)
    }};
    ($mod:expr, (f64_load (offset $o: expr) ( $($ptr: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        $mod.load(4, true, $o, 4, ValueTy::F64, ptr)
    }};
    ($mod:expr, (f64_store (offset $o: expr) ( $($ptr: tt)* ) (  $($val: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        let val = binaryen_expr!($mod, ( $($val)* ));
        $mod.store(8, $o, 8, ptr, val, ValueTy::F64)
    }};
    ($mod:expr, (if_ ( $($cond: tt)* ) (  $($rhs: tt)* ))) => {{
        let cond = binaryen_expr!($mod, ( $($cond)* ));
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.if_(cond, rhs, None)
    }};
    ($mod:expr, (i32_add ( $($lhs: tt)* ) (  $($rhs: tt)* ))) => {{
        let lhs = binaryen_expr!($mod, ( $($lhs)* ));
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.binary(BinaryOp::AddI32, lhs, rhs)
    }};
    ($mod:expr, (i32_mul ( $($lhs: tt)* ) (  $($rhs: tt)* ))) => {{
        let lhs = binaryen_expr!($mod, ( $($lhs)* ));
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.binary(BinaryOp::MulI32, lhs, rhs)
    }};
    ($mod:expr, (i32_shl ( $($lhs: tt)* ) (  $($rhs: tt)* ))) => {{
        let lhs = binaryen_expr!($mod, ( $($lhs)* ));
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.binary(BinaryOp::ShlI32, lhs, rhs)
    }};
    ($mod:expr, (i32_lt_u ( $($lhs: tt)* ) (  $($rhs: tt)* ))) => {{
        let lhs = binaryen_expr!($mod, ( $($lhs)* ));
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.binary(BinaryOp::LtUI32, lhs, rhs)
    }};
    ($mod:expr, (br $label: expr)) => {
        $mod.break_($label, None, None)
    };
    ($mod:expr, (br_if ($label: expr) ( $($cond: tt)* ))) => {{
        let cond = binaryen_expr!($mod, ( $($cond)* ));
        $mod.break_($label, Some(cond), None)
    }};
    ($mod:expr, block[ $($e:tt)* ]) => {{
        let exprs = vec![
            $(binaryen_expr!($mod, $e),)*
        ];

        $mod.block::<&'static str, _>(None, exprs, None)
    }};
    ($mod:expr, (loop_ ($label:expr) [ $($e:tt)* ])) => {{
        let exprs = vec![
            $(binaryen_expr!($mod, $e),)*
        ];

        let body = $mod.block::<&'static str, _>(None, exprs, None);
        $mod.loop_($label, body)
    }}
}

fn _alloc1d(module: &Module) -> Expr {
    binaryen_expr! {
        module,
        block[
            (i32_store (offset 0)
                 (i32_tee_local (2)
                      (i32_load (offset 0) (i32_get_local 0))
                 )
                 (i32_const 1)
            )
            (i32_store (offset 4)
                 (i32_get_local 2)
                 (i32_tee_local (3)
                      (i32_add
                       (i32_get_local 2)
                       (i32_const 16))
                 )
            )
            (if_ (i32_get_local 1)
                (loop_ ("$label$2") [
                    (i64_store (offset 0)
                         (i32_get_local 3)
                         (i64_const 0)
                    )
                    (i32_set_local (3)
                         (i32_add (i32_get_local 3) (i32_const 8))
                    )
                    (br_if ("$label$2")
                         (i32_lt_u
                              (i32_tee_local (4)
                                   (i32_add (i32_get_local 4) (i32_const 1))
                              )
                              (i32_get_local 1)
                         )
                    )
                ])
            )
            (i32_store (offset 0)
                 (i32_get_local 0)
                 (i32_add
                     (i32_add (i32_get_local 2)
                          (i32_shl (i32_get_local 1)
                                   (i32_const 3))
                     )
                     (i32_const 16)
                 )
            )
            (i32_get_local 2)
        ]
    }
}

fn _load1d(module: &Module) -> Expr {
    binaryen_expr! {module,
        (f64_load (offset 0)
            (i32_add
                (i32_load (offset 4) (i32_get_local 0))
                (i32_shl
                      (i32_mul
                            (i32_load (offset 0)
                                 (i32_get_local 0))
                            (i32_get_local 1)
                      )
                      (i32_const 3)
                )
            )
        )
    }
}

fn _load2d(module: &Module) -> Expr {
    binaryen_expr! {module,
        (f64_load (offset 0)
            (i32_add
                (i32_load (offset 8) (i32_get_local 0))
                (i32_shl
                      (i32_add
                          (i32_mul
                                (i32_load (offset 0)
                                      (i32_add (i32_get_local 0) (i32_const 4))
                                )
                                (i32_get_local 2)
                          )
                          (i32_mul
                                (i32_load (offset 0) (i32_get_local 0))
                                (i32_get_local 1)
                          )
                     )
                     (i32_const 3)
                )
            )
        )
    }
}
