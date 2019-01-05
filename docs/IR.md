# IR

## Example

Here is an example BASIC program.

```visual basic
10 READ X, Y, Z
20 DATA 1, 2, 3
30 FOR I = 1 TO 10
40   GOSUB 100
50 NEXT I
99 END

100 LET X = X + Y^Z
110 LET Z = X
120 RETURN
```

After parsing, it is compiled to an intermediate representation, printed below.

```
globals: Var(Z) Var(Y) Var(X)

function fn_0
  defined on Line: 100
  type: fn()
  L0
    X <- (X + Y ^ Z)
    Z <- X
    return
function main
  defined on Line: 10
  type: fn()
  local(0): f64
  L1
    $0 <- 0
    X <- read()
    Y <- read()
    Z <- read()
    goto L2
  L2
    $0 <- 1
    goto L3
  L3
    if ($0 - 10) > 0:
      goto L6
    else:
      goto L4
  L4
    fn_0();
    goto L5
  L5
    $0 <- ($0 + 1)
    goto L3
  L6
    return
```

## Design Goals

IR is still a tree form, compared to `AST`, it achieves the following:

1. Models BASIC program as functions

   * Extracts subroutines as functions
   * Wrap the program as a `main` function
   * Generates functions for `DEF` statements (not shown above)

2. Group statements into basic blocks

3. Compiles `FOR` loops into jumps and conditional jumps

   * Some non-lexical `FOR`s a re supported, provided that each `FOR` has exactly one reachable `NEXT` statement, which means this program compiles (for better or for worse):

   ```visual basic
   10 FOR I = 1 TO 5
   20 FOR J = 1 TO 5
   30 PRINT "I="I, "J=" J
   40 NEXT I
   50 NEXT J
   ```

4. Limited optmizations:

   * Local constant folding, its aim is to make common `FOR` loops with fixed step and target more efficient
     * A general form of `FOR` loop such as `10 FOR I = RND(X) TO RND(X) STEP RND(X)` requires additional local variables and more complicated conditional check instructions
   * Global variable demotion, if a global variable is not used across functions, its replaced by a local variable in the function is is referenced/assigned to. In the above example, variable `I` is demoted in `main`.
     * This is very useful for `wasm` compilation, as by lowering global variables, `binaryen` can perform more optimizations
   * Additional optimizations might be added as I learn more about optimizing compilers, particularly, constant propagation across basic blocks and liveness analysis would be useful (for the interpreter VM only, `binaryen` performs all of these)



Some valid BASIC program with irreducible control flow may get rejected - for instance jumping inside a subroutine. These programs can be interpreted just fine, however, they complicate compilation an awful low. As a trade off, these cases are rejected.



## Further Assumptions

At IR level, the following are assumed to be foreign apis provided by runtime:

* IO: `PRINT`  statements
* IO: `RAND`, random `f64` generation in `[0, 1)` interval
* Array allocations: runtime environment should support:
  * `alloc1d $X <expr>`
  * `alloc2d $Y <expr>, <expr>`
* `READ`:  runtime env should collect numbers defined in `DATA` , and track how many values have been consumed as the program is executing

## Codegen

This form of IR is cobbled together to make `binaryen` powered `wasm` codegen easier, statements and expressions are trivially mapped to `binaryen` `Expr`s. And branching can be easily recreated using `Relooper`.



For stack `VM` codegen in main crate, it's also fairly easy - even if it's a bit unnecessary. Emitting code directly from `AST` is straightforward, and does not require control flow analysis to handl



