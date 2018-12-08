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



Some sample disassembler output. (Source of this program is `sample_programs/func_redefine.bas`).

```
10    0000    noop      
15    0001    fn         <compiled function 1>
 |    0004    set.fn     FNZ
20    0007    fn         <compiled function 2>
 |    0010    set.fn     FNA
25    0013    const      10
 |    0016    get.fn     FNA
 |    0019    call      
 |    0020    set.var    Y1
30    0023    prt       
 |    0024      prt.lab  "FNA(10) ="
 |    0027      prt;    
 |    0028      get.var  Y1
 |    0031      prt.expr
 |    0032    prt.end   
40    0033    fn         <compiled function 3>
 |    0036    set.fn     FNA
45    0039    const      10
 |    0042    get.fn     FNA
 |    0045    call      
 |    0046    set.var    Y2
50    0049    prt       
 |    0050      prt.lab  "FNA(10) ="
 |    0053      prt;    
 |    0054      get.var  Y2
 |    0057      prt.expr
 |    0058    prt.end   
60    0059    get.var    Y1
 |    0062    get.var    Y2
 |    0065    eq        
 |    0066    not       
 |    0067    jmp.t      76
70    0070    prt       
 |    0071      prt.lab  "FAILED"
 |    0074    prt.end   
80    0075    stop      
100   0076    prt       
 |    0077      prt.lab  "Ok"
 |    0080    prt.end   
110   0081    stop      
 |    0082    stop      

Chunk: <compiled function 1>

15    0000    set.loc    X
 |    0003    get.loc    X
 |    0006    ret       

Chunk: <compiled function 2>

20    0000    set.loc    X
 |    0003    const      1
 |    0006    get.loc    X
 |    0009    add       
 |    0010    ret       

Chunk: <compiled function 3>

40    0000    set.loc    X
 |    0003    get.loc    X
 |    0006    get.fn     FNZ
 |    0009    call      
 |    0010    const      1
 |    0013    sub       
 |    0014    ret      
```
