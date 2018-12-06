# `basic_rs` : a BASIC Interpreter for the Original Dartmouth Version



A BASIC language interpreter written in `rust`. This project is motivated and inspired by Peter Norvig's [BASIC interpreter in python](http://nbviewer.jupyter.org/github/norvig/pytudes/blob/master/ipynb/BASIC.ipynb), reading that notebook helped me tremendously.



## Features and Limitations

Matches first version of [Dartmouth Basic](https://en.wikipedia.org/wiki/Dartmouth_BASIC) closely: reference manual [here](http://web.archive.org/web/20120716185629/http://www.bitsavers.org/pdf/dartmouth/BASIC_Oct64.pdf), which means this implementation inherits all its limitations.

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



Some sample disassembler output.

```
550   0538    get.var    <anonymous variable 06>
 |    0541    dup       
 |    0542    sign      
 |    0543    swap      
 |    0544    get.var    Y
 |    0547    add       
 |    0548    dup       
 |    0549    set.var    Y
 |    0552    get.var    <anonymous variable 07>
 |    0555    sub       
 |    0556    sign      
 |    0557    eq        
 |    0558    jmp.f      479
560   0561    ret       
700   0562    noop      
705   0563    prt       
 |    0564      prt.lab  "GEN "
 |    0567      get.var  I
 |    0570      prt.expr
 |    0571    prt.end   
710   0572    const      1
 |    0575    set.var    Y
 |    0578    get.var    M
 |    0581    set.var    <anonymous variable 011>
 |    0584    const      1
 |    0587    set.var    <anonymous variable 010>
```

(`<anonymous variable>`s are used by compiler to implement `FOR` loops.. based on a particularly chosen `FOR` / `NEXT` semantics).