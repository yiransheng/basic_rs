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



For stack `VM` codegen in main crate, it's also fairly easy - even if it's a bit unnecessary. Emitting code directly from `AST` is straightforward, and does not require control flow analysis to handle non-lexical `FOR`s and `GOSUB` as functions. In fact, earlier version of `basic_rs` runs exactly that way.



## How it's Done

Roughly like so:

### Step 1: Control Flow Context

Program AST is traversed in a `DFS` order, each line of statement is considered as a node in CFG graph, and assigned a label and function id.`GOTO` and `IF` statements adds branches (edges) in the obvious manner. The target line of `GOTO` and `IF` gets marked with a new label.

In this stage, `FOR` and `NEXT` are considered non-branching statements, and do not add back edges to the CFG. Each `GOSUB` target gets marked as a new function, and any reachable line for entry is marked as the same function id, traversal stops when encountering a `RETURN` statement.

### Step 2: Global Defs

Visits the AST to collect all usage/references of global variables, arrays and `DEF` functions. Only caveat is the visitor needs to be aware of `DEF` local variable shadowing, eg. `DEF FNZ(X) = X + 1` does not introduce a global variable named `X`.

### Step 3: Non-loop pass

First create functions and all their basic blocks (initialized to be empty), based on information collected in step 1; this is doable since each line has been assigned labels and function ids. Afterwards traverse the AST in line order, and look up corresponding function and block for each line, and push statements into it. Unreachable code raises a compile error as of now.

Most statements are translated mechanically via pattern matching, `GOSUB` becomes a `CALL`  IR statement for example. For `IF` and `GOTO`, lookup the basic block label based on line number, and assign it to current basic block's exit property.

`FOR` and `NEXT` are skipped.

### Step 4: Loop pass

In this pass, AST are traversed not in line order. Rather, for each `FOR` statement, compile it into something akin to:

```visual basic
10 FOR I = 1 TO 10
```

becomes:

```visual basic
10   LET I = 1
10.1 IF I - 10 > 0 THEN ???
```

After this, traverse all reachable lines or current `FOR` line (following back edges as well), until a `NEXT` statement with the same index variable (`I` in this case) is encountered, and matches it to the `FOR` statement, filling the `???` part above as the label of the basic block succeeding this `NEXT`. The traversal does not stop here, it continues until all reachable lines of current `FOR` has been examined, suppose there is another `NEXT` with the same index variable, a compile error is raised.

A loose end here is extra `NEXT`s, since the compiler only examines `FOR` statements, a rouge `NEXT` not matched with any `FOR`s will simply be ignored (effectively becomes a `Nop`), if it is not reachable by any `FOR`s. 



