mod anon_var;
mod array_dims;
mod compiler;
mod data;
mod error;
// mod func_compiler;
mod ir_labels;
mod line_order;

pub use self::compiler::{CompileError, Compiler, Target};

use crate::ast;
use crate::vm::from_ir::{ChunkWriter, WriteError};
use crate::vm::VM;

impl Into<self::error::CompileError> for WriteError {
    fn into(self) -> self::error::CompileError {
        self::error::CompileError::Custom(
            "Compile error, failed to write byte code",
        )
    }
}

pub fn compile(ast: &ast::Program) -> Result<VM, CompileError> {
    let compiler: Compiler<Target<ChunkWriter>> = Compiler::new();
    let (main_id, chunks) = compiler.compile(ast)?;

    Ok(VM::new(main_id, chunks))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::*;

    use crate::parser::Parser;
    use crate::scanner::Scanner;

    #[test]
    fn test_simple() {
        let program = indoc!(
            "
            10 LET X = SIN(10)
            15 DIM X(20, 20), Y(15)
            20 READ X, Y, A
            21 FOR I = 0 TO 14
            22   LET Y(I) = I + 1000
            23 NEXT I
            24 FOR I = 10 TO 1 STEP -1
            25   PRINT Y(I)
            26 NEXT I
            31 DEF FNA(D) = D + 10
            32 DEF FNB(X) = X + FNA(X) + Y
            40 PRINT \"FNB(Y)\", FNB(Y)
            50 REM IGNORE ME
            55 LET X(3, 3) = 99
            60 GOSUB 100
            70 PRINT \"X(3, 3)\"; X(3, 3)
            80 GOTO 800
            100 REM SUBROUTING
            120 PRINT \"拼音\", A
            130 RETURN
            150 DATA 10, 20, 30
            800 END"
        );
        let printed = indoc!(
            "
            1010
            1009
            1008
            1007
            1006
            1005
            1004
            1003
            1002
            1001
            FNB(Y)         70
            拼音           30
            X(3, 3)  99
            "
        );

        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut vm = compile(&ast).unwrap();

        let mut output = Vec::new();
        vm.run(&mut output).expect("no runtime error");

        assert_eq!(::std::str::from_utf8(&output), Ok(printed));
    }
}
