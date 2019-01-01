use binaryen::*;

macro_rules! binaryen_expr {
    ($mod: expr, (escaped $e: expr)) => {
        $e
    };
    // constants
    ($mod: expr, (i32_const $e: expr)) => {
        $mod.const_(Literal::I32($e))
    };
    ($mod: expr, (i64_const $e: expr)) => {
        $mod.const_(Literal::I64($e))
    };
    // locals
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
    // load and store
    ($mod:expr, (i32_load (bytes=$b:expr, offset=$o:expr, align=$a:expr) ( $($ptr: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        $mod.load($b, true, $o, $a, ValueTy::I32, ptr)
    }};
    ($mod:expr, (i32_store (bytes=$b:expr, offset=$o:expr, align=$a:expr) ( $($ptr: tt)* ) (  $($val: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        let val = binaryen_expr!($mod, ( $($val)* ));
        $mod.store($b, $o, $a, ptr, val, ValueTy::I32)
    }};
    ($mod:expr, (i64_load (bytes=$b:expr, offset=$o:expr, align=$a:expr) ( $($ptr: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        $mod.load($b, true, $o, $a, ValueTy::I64, ptr)
    }};
    ($mod:expr, (i64_store (bytes=$b:expr, offset=$o:expr, align=$a:expr) ( $($ptr: tt)* ) (  $($val: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        let val = binaryen_expr!($mod, ( $($val)* ));
        $mod.store($b, $o, $a, ptr, val, ValueTy::I64)
    }};
    ($mod:expr, (f64_load (bytes=$b:expr, offset=$o:expr, align=$a:expr) ( $($ptr: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        $mod.load($b, true, $o, $a, ValueTy::F64, ptr)
    }};
    ($mod:expr, (f64_store (bytes=$b:expr, offset=$o:expr, align=$a:expr) ( $($ptr: tt)* ) (  $($val: tt)* ))) => {{
        let ptr = binaryen_expr!($mod, ( $($ptr)* ));
        let val = binaryen_expr!($mod, ( $($val)* ));
        $mod.store($b, $o, $a, ptr, val, ValueTy::F64)
    }};
    // if
    ($mod:expr, (if_ ( $($cond: tt)* ) (  $($rhs: tt)* ))) => {{
        let cond = binaryen_expr!($mod, ( $($cond)* ));
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.if_(cond, rhs, None)
    }};
    // unary ops
    ($mod:expr, (i64_extend_u_i32 (  $($rhs: tt)* ))) => {{
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.unary(UnaryOp::ExtendUI32, rhs)
    }};
    // binary ops
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
    ($mod:expr, (i64_or ( $($lhs: tt)* ) (  $($rhs: tt)* ))) => {{
        let lhs = binaryen_expr!($mod, ( $($lhs)* ));
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.binary(BinaryOp::OrI64, lhs, rhs)
    }};
    ($mod:expr, (i64_shl ( $($lhs: tt)* ) (  $($rhs: tt)* ))) => {{
        let lhs = binaryen_expr!($mod, ( $($lhs)* ));
        let rhs = binaryen_expr!($mod, ( $($rhs)* ));
        $mod.binary(BinaryOp::ShlI64, lhs, rhs)
    }};
    // br
    ($mod:expr, (br $label: expr)) => {
        $mod.break_($label, None, None)
    };
    ($mod:expr, (br_if ($label: expr) ( $($cond: tt)* ))) => {{
        let cond = binaryen_expr!($mod, ( $($cond)* ));
        $mod.break_($label, Some(cond), None)
    }};
    // loop and block
    ($mod:expr, (block[ $($e:tt)* ])) => {{
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

pub fn runtime_api(module: &Module) {
    alloc1d(module);
    store1d(module);
    alloc2d(module);
}

fn alloc1d(module: &Module) {
    let locals = vec![ValueTy::I32; 3];
    let ty = module.add_fn_type(
        Some("alloc1d_"),
        // ptr, size
        &[ValueTy::I32, ValueTy::I32],
        Ty::I32, // ptr
    );
    let body = _alloc1d(&module);

    module.add_fn("alloc1d_", &ty, &locals, body);
}
fn store1d(module: &Module) {
    let ty = module.add_fn_type(
        Some("store1d_"),
        // ptr, index
        &[ValueTy::I32, ValueTy::I32, ValueTy::F64],
        Ty::None,
    );
    let body = _store1d(&module);

    module.add_fn("store1d_", &ty, &[], body);
}
fn alloc2d(module: &Module) {
    let locals = vec![ValueTy::I32; 3];
    let ty = module.add_fn_type(
        Some("alloc2d_"),
        // ptr, row, col
        &[ValueTy::I32, ValueTy::I32, ValueTy::I32],
        Ty::I32, // ptr
    );
    let body = _alloc2d(&module);

    module.add_fn("alloc2d_", &ty, &locals, body);
}

fn _alloc1d(module: &Module) -> Expr {
    binaryen_expr! {
        module,
        (block[
            (i32_store (bytes=4, offset=0, align=4)
                 (i32_tee_local (2)
                      (i32_load (bytes=4, offset=0, align=4) (i32_get_local 0))
                 )
                 (i32_const 1)
            )
            (i32_store (bytes=4, offset=0, align=4)
                 (i32_get_local 2)
                 (i32_tee_local (3)
                      (i32_add
                       (i32_get_local 2)
                       (i32_const 16))
                 )
            )
            (if_ (i32_get_local 1)
                (loop_ ("$label$2") [
                    (i64_store (bytes=8, offset=0, align=8)
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
            (i32_store (bytes=4, offset=0, align=4)
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
        ])
    }
}

fn _load1d(module: &Module) -> Expr {
    binaryen_expr! {module,
        (f64_load (bytes=8, offset=0, align=8)
            (i32_add
                (i32_load (bytes=4, offset=4, align=4) (i32_get_local 0))
                (i32_shl
                      (i32_mul
                            (i32_load (bytes=4, offset=0, align=4)
                                 (i32_get_local 0))
                            (i32_get_local 1)
                      )
                      (i32_const 3)
                )
            )
        )
    }
}

fn _store1d(module: &Module) -> Expr {
    binaryen_expr! {module,
        (f64_store (bytes=8, offset=0, align=8)
            (i32_add
                (i32_load (bytes=4, offset=4, align=4)
                    (i32_get_local 0)
                )
                (i32_shl
                    (i32_mul
                          (i32_load (bytes=4, offset=0, align=4)
                               (i32_get_local 0))
                          (i32_get_local 1)
                    )
                    (i32_const 3)
                )
            )
            (i32_get_local 2)
        )
    }
}

fn _alloc2d(module: &Module) -> Expr {
    binaryen_expr! {module, (block[
        (i32_store (bytes=4, offset=8, align=4)
            (i32_tee_local (3)
                (i32_load (bytes=4, offset=0, align=4) (i32_get_local 0))
            )
            (i32_tee_local (4)
                (i32_add (i32_get_local 3) (i32_const 16))
            )
        )
        (i64_store (bytes=8, offset=0, align=4)
            (i32_get_local 3)
            (i64_or
                (i64_shl
                     (i64_extend_u_i32 (i32_get_local 1))
                     (i64_const 32)
                )
                (i64_const 1)
            )
        )
        (i32_set_local (5)
            (i32_shl
                 (i32_tee_local (2)
                      (i32_mul (i32_get_local 2) (i32_get_local 1))
                 )
                 (i32_const 3)
            )
        )
        (if_ (i32_get_local 2)
             (block [
                  (i32_set_local (1) (i32_const 0))
                  (loop_ ("$label$2") [
                      (i64_store (bytes=8, offset=0, align=8)
                           (i32_get_local 4)
                           (i64_const 0)
                      )
                      (i32_set_local (4)
                           (i32_add (i32_get_local 4) (i32_const 8))
                      )
                      (br_if ("$label$2")
                           (i32_lt_u
                               (i32_tee_local (1)
                                    (i32_add
                                     (i32_get_local 1)
                                     (i32_const 1)))
                               (i32_get_local 2)
                           )
                      )
                  ])
             ])
        )
    ])}
}

fn _load2d(module: &Module) -> Expr {
    binaryen_expr! {module,
        (f64_load (bytes=8, offset=0, align=8)
            (i32_add
                (i32_load (bytes=4, offset=8, align=4) (i32_get_local 0))
                (i32_shl
                      (i32_add
                          (i32_mul
                                (i32_load (bytes=4, offset=0, align=4)
                                      (i32_add (i32_get_local 0) (i32_const 4))
                                )
                                (i32_get_local 2)
                          )
                          (i32_mul
                                (i32_load (bytes=4, offset=0, align=4) (i32_get_local 0))
                                (i32_get_local 1)
                          )
                     )
                     (i32_const 3)
                )
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_macro() {
        let mut module = Module::new();
        let ty = module.add_fn_type(
            Some("load1d"),
            &[ValueTy::I32, ValueTy::I32],
            Ty::F64,
        );

        let body = _load1d(&module);

        module.add_fn("load1d", &ty, &[], body);

        module.print();
    }
}
