use std::fmt;
use std::io::{self, Write};

use crate::ir::*;
use crate::relooper::Relooper;

trait ToJs {
    type Error;

    fn codegen<'a, W: Write>(
        &'a self,
        program: &Program,
    ) -> Result<fmt::Arguments<'a>, Self::Error>;
}

impl ToJs for Expr {
    type Error = ();

    fn codegen<'a, W: Write>(
        &'a self,
        program: &Program,
    ) -> Result<fmt::Arguments<'a>, Self::Error> {
        let code = match self {
            Expr::RandF64 => format_args!("Math.random()"),
            Expr::ReadData => unimplemented!(),
            Expr::Input => unimplemented!(),
            Expr::Const(v) => format_args!("{}", v),
            Expr::Get(lval) => lval.codegen(program, out)?,
            Expr::Unary(op, operand) => match op {
                UnaryOp::Not => format_args!("!{}", operand.codegen(program)?),
                UnaryOp::Neg => {
                    format_args!("(-{})", operand.codegen(program)?)
                }
                UnaryOp::Sin => {
                    format_args!("Math.sin({})", operand.codegen(program)?)
                }
                UnaryOp::Cos => {
                    format_args!("Math.cos({})", operand.codegen(program)?)
                }
                UnaryOp::Atn => {
                    format_args!("Math.atan({})", operand.codegen(program)?)
                }
                UnaryOp::Exp => {
                    format_args!("Math.exp({})", operand.codegen(program)?)
                }
                UnaryOp::Abs => {
                    format_args!("Math.abs({})", operand.codegen(program)?)
                }
                UnaryOp::Log => {
                    format_args!("Math.log({})", operand.codegen(program)?)
                }
                UnaryOp::Sqr => {
                    format_args!("Math.sqrt({})", operand.codegen(program)?)
                }
                UnaryOp::Trunc => {
                    format_args!("Math.trunc({})", operand.codegen(program)?)
                }
            },
            Expr::Binary(op, lhs, rhs) => match op {
                BinaryOp::Add => format_args!(
                    "({} + {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::Sub => format_args!(
                    "({} - {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::Mul => format_args!(
                    "({} * {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::Div => format_args!(
                    "({} / {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::Rem => format_args!(
                    "({} % {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::Pow => format_args!(
                    "Math.pow({}, {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::Less => format_args!(
                    "({} < {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::Greater => format_args!(
                    "({} > {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::Equal => format_args!(
                    "({} === {})",
                    lhs.codegen(program)?,
                    rhs.codegen(program)?,
                ),
                BinaryOp::CopySign => unimplemented!(),
            },
        };

        Ok(code)
    }
}
