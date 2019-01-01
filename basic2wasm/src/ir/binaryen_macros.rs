use binaryen::*;

#[macro_export]
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
