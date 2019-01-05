# `basic_rs` : a BASIC Interpreter for the Original Dartmouth Version

[![Build Status](https://travis-ci.org/yiransheng/basic_rs.svg?branch=master)](https://travis-ci.org/yiransheng)


A BASIC language interpreter written in `rust`. This project is motivated and inspired by Peter Norvig's [BASIC interpreter in python](http://nbviewer.jupyter.org/github/norvig/pytudes/blob/master/ipynb/BASIC.ipynb), reading that notebook helped me tremendously.



## Features and Limitations

Matches first version of [Dartmouth Basic](https://en.wikipedia.org/wiki/Dartmouth_BASIC) closely: reference manual [here](http://web.archive.org/web/20120716185629/http://www.bitsavers.org/pdf/dartmouth/BASIC_Oct64.pdf), which means this implementation inherits all its limitations.

* [**Update**] Has an additional crate `basic2wasm` to compile BASIC programs to WebAssembly!
  * Still hacky and buggy, but it is able to compile  [Game of Life](http://nbviewer.jupyter.org/github/norvig/pytudes/blob/master/ipynb/BASIC.ipynb), see it running [here](http://subdued-afternoon.surge.sh/)
* No input support other than `DATA` statements in source program
* No string / boolean value types, only value type is `f64`
* Variable names restricted to `[A-Z]\d?` 
* Function names restricted to `FN[A-Z]`
* List and table dimension restrictions
* Otherwise supports all 15 types of statements: `LET`, `READ`, `DATA`, `PRINT`, `GOTO`, `IF`, `FOR`, `NEXT`, `END`, `STOP`, `DEF`, `GOSUB`, `RETURN`, `DIM` and `REM`



## Run Program

```shell
basic_rs ./my_program.bas 
```

Optionally, `-d` flag disassembles compiled VM byte code:

```
basic_rs -d ./debug.bas
```



## Implementation Details

Compared to Norvig's implementation, the flavor of this project is more of no-hack, from-scratch approach (Norvig's version leveraged many of Python's powerful, dynamic features to get things done fast and cleverly). My goal was trying to learn how to implement a simple language as principled as I could manage.



BASIC source code is _lexed_, _parsed_ and _compiled_ into a custom stack based VM byte code (consist of instructions I made up somewhat arbitrarily along the way instead of properly designed).



### VM features:

* standard stack based arithmetics
* global variables and arrays
* function call with 0 or 1 argument
  * the former is used for subroutine calls
  * the latter is used for user defined function calls `FNA` - `FNZ`
* custom IO instructions to match BASIC's weird print semantics



Some sample disassembler output. (Source of this program is `sample_programs/func_redefine.bas`).

```
10    0000    decl.loc   2
15    0002    const      0
 |    0005    set.loc    $0
20    0008    const      0
 |    0011    set.loc    $1
25    0014    bind.fn    FNZ <compiled function 0>
30    0019    bind.fn    FNA <compiled function 1>
 |    0024    const      10
 |    0027    get.fn     FNA
 |    0030    call_      args: 1
 |    0032    set.loc    $0
 |    0035    prt.lab    "FNA(10) ="
 |    0038    prt;      
40    0039    get.loc    $0
 |    0042    prt.expr  
45    0043    prt\n     
50    0044    bind.fn    FNA <compiled function 2>
 |    0049    const      10
 |    0052    get.fn     FNA
 |    0055    call_      args: 1
 |    0057    set.loc    $1
 |    0060    prt.lab    "FNA(10) ="
 |    0063    prt;      
 |    0064    get.loc    $0
 |    0067    get.loc    $1
 |    0070    eq        
 |    0071    not       
 |    0072    jmp.t      80
70    0075    prt.lab    "FAILED"
 |    0078    prt\n     
 |    0079    ret       
100   0080    prt.lab    "Ok"
 |    0083    prt\n     
 |    0084    ret       

Chunk: <compiled function 0>

15    0000    decl.loc   0
 |    0002    get.loc    $0
 |    0005    ret.val   

Chunk: <compiled function 2>

40    0000    decl.loc   0
 |    0002    get.loc    $0
 |    0005    get.fn     FNZ
 |    0008    call_      args: 1
 |    0010    const      1
 |    0013    sub       
 |    0014    ret.val   

Chunk: <compiled function 1>

20    0000    decl.loc   0
 |    0002    const      1
 |    0005    get.loc    $0
 |    0008    add       
 |    0009    ret.val   
```



## WASM

See [README](./basic2wasm/README.md) for `basic2wasm` crate.



## Performance

A simple BASIC program using `RND` to estimate PI via Monte Carlo method is used for benchmarking (using 1000 iterations), and compared to three implementations:

* `python`  (benches/pi.py)

* `nodejs` (benches/pi.js)

* `rust` (`fn` embedded in benchmark file)


Here are the results:

```
interpreter:pi.bas      time:   [388.12 us 389.08 us 390.18 us]                        

extern:pi.py            time:   [202.06 us 203.38 us 205.06 us]                         

extern:pi.js            time:   [19.374 ns 19.605 ns 19.869 ns]                         

rust:pi                 time:   [296.27 ps 296.78 ps 297.42 ps]
```



It is only 2 times slower than `python`. Nodejs is four magnitude (10000 times compared to python) faster, and `rust` (with `target-cpu = "native"`) is pretty much on a different level. There are some minor penalties to node and python versions due to communicating via stdin/stdout. However, the pattern still holds if iteration is increased from 1000 to 1000_000 and without IO barrier.