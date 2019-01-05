use binaryen::*;

pub fn runtime_api(module: &Module) {
    read(module);
    alloc1d(module);
    load1d(module);
    store1d(module);

    alloc2d(module);
    load2d(module);
    store2d(module);
}

fn read(module: &Module) {
    let ty = module.add_fn_type(Some("read"), &[], Ty::F64);
    let body = _read(&module);

    module.add_fn("read", &ty, &[], body);
}

fn alloc1d(module: &Module) {
    let locals = vec![ValueTy::I32; 3];
    let ty = module.add_fn_type(
        Some("alloc1d"),
        // ptr, size
        &[ValueTy::I32, ValueTy::I32],
        Ty::I32, // ptr
    );
    let body = _alloc1d(&module);

    module.add_fn("alloc1d", &ty, &locals, body);
}
fn load1d(module: &Module) {
    let ty = module.add_fn_type(
        Some("load1d"),
        // ptr, index
        &[ValueTy::I32, ValueTy::I32],
        Ty::F64,
    );
    let body = _load1d(&module);

    module.add_fn("load1d", &ty, &[], body);
}
fn store1d(module: &Module) {
    let ty = module.add_fn_type(
        Some("store1d"),
        // ptr, index, value
        &[ValueTy::I32, ValueTy::I32, ValueTy::F64],
        Ty::None,
    );
    let body = _store1d(&module);

    module.add_fn("store1d", &ty, &[], body);
}
fn alloc2d(module: &Module) {
    let locals = vec![ValueTy::I32; 3];
    let ty = module.add_fn_type(
        Some("alloc2d"),
        // ptr, row, col
        &[ValueTy::I32, ValueTy::I32, ValueTy::I32],
        Ty::I32, // ptr
    );
    let body = _alloc2d(&module);

    module.add_fn("alloc2d", &ty, &locals, body);
}
fn load2d(module: &Module) {
    let ty = module.add_fn_type(
        Some("load2d"),
        // ptr, index
        &[ValueTy::I32, ValueTy::I32, ValueTy::I32],
        Ty::F64,
    );
    let body = _load2d(&module);

    module.add_fn("load2d", &ty, &[], body);
}
fn store2d(module: &Module) {
    let ty = module.add_fn_type(
        Some("store2d"),
        // ptr, index
        &[ValueTy::I32, ValueTy::I32, ValueTy::I32, ValueTy::F64],
        Ty::None,
    );
    let body = _store2d(&module);

    module.add_fn("store2d", &ty, &[], body);
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
            (i32_store (bytes=4, offset=4, align=4)
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
            (f64_get_local 2)
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
                      (i32_mul (i32_get_local 1) (i32_get_local 2))
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
        (i32_store (bytes=4, offset=0, align=4)
            (i32_get_local 0)
            (i32_add
                (i32_add (i32_get_local 3) (i32_get_local 5))
                (i32_const 16)
            )
        )
        (i32_get_local 3)
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

fn _store2d(module: &Module) -> Expr {
    binaryen_expr! {module,
        (f64_store (bytes=8, offset=0, align=8)
             (i32_add
                  (i32_load (bytes=4, offset=8, align=4)
                       (i32_get_local 0)
                  )
                  (i32_shl
                       (i32_add
                            (i32_mul
                                 (i32_load (bytes=4, offset=0, align=4)
                                      (i32_add (i32_get_local 0) (i32_const 4))
                                 )
                                 (i32_get_local 2)
                            )
                            (i32_mul
                                 (i32_load (bytes=4, offset=0, align=4)
                                      (i32_get_local 0)
                                 )
                                 (i32_get_local 1)
                            )
                       )
                       (i32_const 3)
                  )
            )
            (f64_get_local 3)
        )
    }
}

fn _read(module: &Module) -> Expr {
    binaryen_expr! {module,
        (if_ (i32_gt_s (i32_get_global "data_end") (i32_const 0))
            (block [
                 (i32_set_global ("data_end")
                      (i32_sub
                           (i32_get_global "data_end")
                           (i32_const 8)
                      )
                 )
                 (f64_load (bytes=8, offset=0, align=8)
                      (i32_get_global "data_end")
                 )
            ])
            (unreachable)
        )
    }
}
