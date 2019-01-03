mod control_flow_context;
mod error;
mod expr_compiler;
mod for_compiler;
mod global_defs;
mod non_loop;

use basic_rs::ast;
use basic_rs::ast::Visitor;

use crate::ir::{Builder, Program};

use self::control_flow_context::CfCtx;
use self::error::CompileError;
use self::for_compiler::LoopPass;
use self::global_defs::GlobalDefPass;
use self::non_loop::NonLoopPass;

pub fn compile(program: &ast::Program) -> Result<Program, CompileError> {
    let mut builder = Builder::new();
    let mut cf_ctx = CfCtx::from_program(program)?;

    GlobalDefPass::new(&mut builder).visit_program(program)?;

    NonLoopPass::new(&cf_ctx, &mut builder).visit_program(program)?;

    LoopPass::new(&mut cf_ctx, &mut builder).visit_program(program)?;

    Ok(builder.build())
}

#[cfg(test)]
mod tests {
    use super::*;
    use basic_rs::{Parser, Scanner};
    use indoc::*;

    #[test]
    fn test_single_for_loop() {
        let program = indoc!(
            "
            10 REM Comment
            20 FOR I = 1 TO 10 STEP 2
            30   GOSUB 60
            40 NEXT I
            50 END
            60 PRINT I 
            70 RETURN"
        );

        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let ir = compile(&ast);

        assert!(ir.is_ok());
    }

    #[test]
    fn test_non_lexical_for_loop() {
        let program = indoc!(
            "
            10 GOTO 50
            20 PRINT I
            30 NEXT I
            40 END
            50 FOR I = 1 TO 10
            60 GOTO 20"
        );

        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let ir = compile(&ast);

        assert!(ir.is_ok());
    }

}
