# `wasm` macro

At some point, I came up with the idea of using `rust` macros to create `binayen` `Expr`  literals with web assembly s-expression syntax directly (idea inspired by `JSX`). Here is how it looks in action:

```rust
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
```

Unfortunately, a few limitations made it not quite the syntax of `wasm` ast form, due to either the differences between `binaryen` api and `wasm` syntax, or `rust`s macro hygiene restrictions (certain tokens not allowed in macros under some conditions). 

* `i32_get_local` / `i32_set_local` instead of `set_local` and `get_local` as in `wast`
  * Due to `binerayen` requiring type info when generating these expressions
* `i32_const` instead of `i32.const`, `f64_add` instead of `f64.add` etc.
* `load` and `store` must specify `align`, `offset` and number of `bytes` fully - and seperated by _**comma**_ instead of spaces...
* `loop` and `block` children uses square brackets `[ ]`, instead of parens `( )`



So in the end it becomes a wasm like DSL with somewhat arbitrary differences, whether it makes rust source code more readable and maintainable is very debatable; in addition, copy outputs from web assembly studio directly into rust source does not work at all. In the end, I kept it, as a cool experiment.