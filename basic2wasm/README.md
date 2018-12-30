# `basic2wasm`

At first, I thought since I already have a stack VM implemented to run BASIC programs, how hard it is to translate it into web assembly code? Turns out, a lot harder than I expected.

Main challenges are two folds:

1. My interpreter VM relies on rust implementations for array allocation and access, `wasm` on the other hand is much lower level, requires everything to be expressed with `f64.load` and `f64.store`
2. `wasm` only allows reducible control flow, and even a trivial can exhibit irreducible structures (jump inside a subroutine)

It took quite a bit of effort, as I was neither familiar with compilers nor web assembly details. Although after this project, I feel I have gained quite a lot understandings for web assembly, as it used almost all aspect of current `wasm` features (basic arithmetics, memory, tables, data segment, dynamic dispatch etc.).

## Overview

At a high level, BASIC concepts are translated into:

* Variable: `wasm` global variable (mutable)
  * Some form of optimization to demote them into local variables if they are not shared across subroutines (`binaryen` could not perform this as of now) would be nice
* Subroutine: `wasm` functions
* `DEF`: currently not supported
* `PRINT`: api provided by `js` as `wasm` function imports
* list / table: `wasm` memory access, implemented using six `wasm` function compiled from unsafe rust (signatures in `rust` syntax, pointers are compiled to `i32` values)
  * `fn alloc1d(next_ptr: *mut *mut u8, size: i32) -> *mut Array<i32>`
  * `fn alloc2d(next_ptr: *mut *mut u8, nrow: i32, ncol: i32) -> *mut Array<[i32, i32]>`
  * `fn load1d(ptr: *mut Array<i32>, index: i32) -> f64`
  * `fn load2d(ptr: *mut Array<[i32, i32]>, i: i32, j: i32) -> f64`
  * `fn store1d(ptr: *mut Array<i32>, index: i32, value: f64)`
  * `fn store2d(ptr: *mut Array<[i32, i32]>, i: i32, j: i32, value: f64)`
  * `Array<Stride>` is an opaque data type for tracking allocation metadata and converting indexes to `f64` pointers
* Requires array dimensions to be specified via `DIM` before usage (different from BASIC spec where list defaults to size 10 and tables 10x10)
* Using arrays is quite unsafe as of now, there are no bound checks, and using uninitialized array pointers (they are just `i32` at runtime) is permitted; running in a browser environment with bad programs will either produce hard to debug logical errors or `wasm` runtime exceptions
* `DATA` statements are compiled into `wasm` `data` segments and placed into `wasm` linear memory location 0
  * A global variable is used to track how much data has been read (starts at the end of data section), decrement by 8 (sizeof `f64`) for every read. When it reaches zero, a `wasm` `unreachable` trap is raised
* labels used in `PRINT` (eg. `PRINT "Hello World"`) are also added to `data` segments
* Next memory location after data and labels is used for book keeping by the array allocation machinery (far from a fully fledged memory allocator, as it does not perform `free`)

## Implementation Notes

The heavy lifting for `wasm` codegen is done by [`binaryen`](https://github.com/WebAssembly/binaryen). Only the the frontend (ast) from main `basic_rs` crate is reused for the `wasm` compiler (I thought I could reuse the stack VM IR there...).

I have made a new form of IR closely matching semantics of `binaryen` IR, with these main features:

* A notion of functions
  * A single `main` function for the main body of the program
  * Subroutines are analyzed and extracted into separated functions
  * Also models `DEF` function and has the concept of function pointers for dynamic dispatching (I have not implemented `wasm` codegen for these yet)

* `FOR` loops are compiled to jump and conditional jumps
* Statements are grouped into basic blocks, and forms a control flow graph (possibly reducible, some really bad ones are rejected here, but I did not spent to much efforts on control flow analysis part, and frankly the code is a bit too messy for myself to fully understand...)

## Example

This simple program for a Monte Carlo estimation of PI:

```basic
100 REM Monte Carlo PI
110 LET C = 0
120 LET N = 1000
230 FOR I = 1 TO N
240   LET X = RND(X)
250   LET Y = RND(X)
260   LET D = X * X + Y * Y
270   IF D > 1 THEN 290
280   LET C = C + 1
290 NEXT I
300 LET P = 4 * C / N
310 PRINT "PI =", P, C"/"N
320 END
```

Produces:

```wast
(module
 (memory $0 1 1)
 (data (i32.const 0) "PI =/")
 (import "env" "print" (func $print (param f64)))
 (import "env" "printLabel" (func $printLabel (param i32 i32)))
 (import "env" "printNewline" (func $printNewline))
 (import "env" "printAdvance15" (func $printAdvance15))
 (import "env" "rand" (func $rand (result f64)))
 ;; omitted runtime functions and types
 (func $main (; 1 ;) (; has Stack IR ;) (type $9)
  (local $0 f64)
  (set_global $C
   (f64.const 0)
  )
  (set_global $N
   (f64.const 1e3)
  )
  (set_local $0
   (get_global $N)
  )
  (set_global $I
   (f64.const 1)
  )
  (block $block$6$break
   (loop $shape$3$continue
    (br_if $block$6$break
     (f64.gt
      (f64.sub
       (get_global $I)
       (get_local $0)
      )
      (f64.const 0)
     )
    )
    (set_global $X
     (call $rand)
    )
    (set_global $Y
     (call $rand)
    )
    (set_global $D
     (f64.add
      (f64.mul
       (get_global $X)
       (get_global $X)
      )
      (f64.mul
       (get_global $Y)
       (get_global $Y)
      )
     )
    )
    (if
     (i32.eqz
      (f64.gt
       (get_global $D)
       (f64.const 1)
      )
     )
     (set_global $C
      (f64.add
       (get_global $C)
       (f64.const 1)
      )
     )
    )
    (set_global $I
     (f64.add
      (get_global $I)
      (f64.const 1)
     )
    )
    (br $shape$3$continue)
   )
  )
  (set_global $P
   (f64.div
    (f64.mul
     (f64.const 4)
     (get_global $C)
    )
    (get_global $N)
   )
  )
  (call $printLabel
   (i32.const 0)
   (i32.const 4)
  )
  (call $printAdvance15)
  (call $print
   (get_global $P)
  )
  (call $printAdvance15)
  (call $print
   (get_global $C)
  )
  (call $printLabel
   (i32.const 4)
   (i32.const 1)
  )
  (call $print
   (get_global $N)
  )
  (call $printNewline)
 )
)
```

For a bigger example with subroutines and arrays, checkout [`./exmaple`](./example) directory. 