mod compiler;
mod control_flow_context;
mod error;
mod expr_compiler;
mod for_compiler;
mod global_defs;
mod non_loop;

use crate::ast;
use crate::scanner::{SourceLoc, SourceMapped};

use crate::ir::{Builder, FunctionName, Label, Program, Statement};

use self::compiler::{Compiler, Pass};
use self::control_flow_context::CfCtx;
use self::for_compiler::LoopPass;
use self::global_defs::GlobalDefPass;
use self::non_loop::NonLoopPass;

pub use self::error::CompileError;

trait HasLineState<E>: ast::Visitor<Result<(), E>> {
    fn line_state(&self) -> usize;

    fn compile(
        &mut self,
        program: &ast::Program,
    ) -> Result<(), SourceMapped<E>> {
        self.visit_program(program).map_err(|err| {
            let line = self.line_state();
            SourceMapped {
                loc: program.statements[line].loc,
                value: err,
            }
        })
    }
}

pub fn compile(
    program: &ast::Program,
) -> Result<Program, SourceMapped<CompileError>> {
    let mut builder = Builder::new();
    let mut cf_ctx =
        CfCtx::from_program(program).map_err(|e| SourceMapped {
            value: CompileError::from(e),
            loc: SourceLoc { line: 0, col: 0 },
        })?;

    // GlobalDefPass::new(&mut builder).compile(program)?;

    let mut pass: Compiler<GlobalDefPass> =
        Compiler::new(&mut cf_ctx, &mut builder);
    pass.compile(program)?;

    let mut pass: Compiler<NonLoopPass> =
        Compiler::new(&mut cf_ctx, &mut builder);
    pass.compile(program)?;

    LoopPass::new(&mut cf_ctx, &mut builder).compile(program)?;

    Ok(builder.build())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Parser, Scanner};
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
