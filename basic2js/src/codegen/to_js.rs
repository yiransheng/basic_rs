use std::io::{self, Write};

use super::{JsCode, ToJs};
use basic_rs::ir::*;

impl ToJs for Statement {
    type Error = io::Error;

    fn codegen<'a, W: Write>(
        &'a self,
        js: &mut JsCode<W>,
    ) -> Result<(), Self::Error> {
        match self {
            Statement::Assign(lval, expr) => {
                lval.codegen(js)?;
                js.write(" = ")?;
                expr.codegen(js)?;
            }
            Statement::DefFn(lval, func) => {
                lval.codegen(js)?;
                js.write(" = ")?;
                let name = js.function_name_string(*func);
                js.write(name)?;
            }
            Statement::CallSub(name) => {
                let name = js.function_name_string(*name);
                js.write(format_args!("yield* {}()", name))?;
            }
            Statement::Alloc1d(..) => {
                // do nothing
            }
            Statement::Alloc2d(..) => {
                // do nothing
            }
            Statement::Print(expr) => {
                js.write_group("env.print(", ")", |js| expr.codegen(js))?;
            }
            Statement::PrintLabel(offset, len) => {
                let s = ::std::str::from_utf8(
                    &js.ir.labels.as_bytes()[*offset..*offset + *len],
                )
                .unwrap();
                // not need to escape, BASIC source does not support escaping double quote, so any
                // string that needs escaping will fail at Parser
                js.write_group("env.printLabel(\"", "\")", |js| js.write(s))?;
            }
            Statement::PrintAdvance3 => {
                js.write("env.printAdvance3()")?;
            }
            Statement::PrintAdvance15 => {
                js.write("env.printAdvance15()")?;
            }
            Statement::PrintNewline => {
                js.write("env.printNewline()")?;
            }
        }

        js.writeln(";")?;

        Ok(())
    }
}

impl ToJs for GlobalKind {
    type Error = io::Error;

    fn codegen<'a, W: Write>(
        &'a self,
        js: &mut JsCode<W>,
    ) -> Result<(), Self::Error> {
        match self {
            GlobalKind::Variable(var) => {
                js.writeln(format_args!("var {} = 0;", var))
            }
            GlobalKind::ArrPtr(var, _) => js.writeln(format_args!(
                "var ARRAY_{} = new SparseArray();",
                var
            )),
            GlobalKind::FnPtr(var) => {
                js.writeln(format_args!("var {} = null;", var))
            }
        }
    }
}

impl ToJs for LValue {
    type Error = io::Error;

    fn codegen<'a, W: Write>(
        &'a self,
        js: &mut JsCode<W>,
    ) -> Result<(), Self::Error> {
        match self {
            LValue::ArrPtr(var, offset) => match offset {
                Offset::OneD(i) => {
                    js.write(format_args!("ARRAY_{}.index1d(", var))?;
                    i.codegen(js)?;
                    js.write(").value")?;
                }
                Offset::TwoD(i, j) => {
                    js.write(format_args!("ARRAY_{}.index2d(", var))?;
                    i.codegen(js)?;
                    js.write(", ")?;
                    j.codegen(js)?;
                    js.write(").value")?;
                }
            },
            LValue::FnPtr(func) => js.write(func)?,
            LValue::Global(var) => js.write(var)?,
            LValue::Local(index) => js.write(format_args!("$x{}", index))?,
        }

        Ok(())
    }
}

impl ToJs for Expr {
    type Error = io::Error;

    fn codegen<'a, W: Write>(
        &'a self,
        js: &mut JsCode<W>,
    ) -> Result<(), Self::Error> {
        macro_rules! unary {
            ($op: expr, $operand: expr) => {{
                js.write("( ")?;

                js.write($op)?;
                $operand.codegen(js)?;

                js.write(" )")?;
            }};
        }

        macro_rules! call {
            ($fn: expr, $operand: expr) => {{
                js.write($fn)?;
                js.write("( ")?;

                $operand.codegen(js)?;

                js.write(" )")?;
            }};
        }

        macro_rules! binary {
            ($lhs: expr, $op: expr, $rhs: expr) => {{
                js.write("( ")?;
                $lhs.codegen(js)?;

                js.write($op)?;

                $rhs.codegen(js)?;
                js.write(" )")?;
            }};
        }

        match self {
            Expr::RandF64 => js.write("Math.random()")?,
            Expr::ReadData => js.write("env.read()")?,
            Expr::Input => js.write("yield env.input()")?,
            Expr::Call(func, expr) => {
                func.codegen(js)?;
                js.write_group("(", ")", |js| expr.codegen(js))?;
            }
            Expr::Const(v) => js.write(v)?,
            Expr::Get(lval) => lval.codegen(js)?,
            Expr::Unary(op, operand) => match op {
                UnaryOp::Not => unary!("!", operand),
                UnaryOp::Neg => unary!("-", operand),
                UnaryOp::Sin => call!("Math.sin", operand),
                UnaryOp::Cos => call!("Math.cos", operand),
                UnaryOp::Atn => call!("Math.atan", operand),
                UnaryOp::Exp => call!("Math.exp", operand),
                UnaryOp::Abs => call!("Math.abs", operand),
                UnaryOp::Log => call!("Math.log", operand),
                UnaryOp::Sqr => call!("Math.sqrt", operand),
                UnaryOp::Trunc => call!("Math.trunc", operand),
            },
            Expr::Binary(op, lhs, rhs) => match op {
                BinaryOp::Add => binary!(lhs, "+", rhs),
                BinaryOp::Sub => binary!(lhs, "-", rhs),
                BinaryOp::Mul => binary!(lhs, "*", rhs),
                BinaryOp::Div => binary!(lhs, "/", rhs),
                BinaryOp::Rem => binary!(lhs, "%", rhs),
                BinaryOp::Less => binary!(lhs, "<", rhs),
                BinaryOp::Greater => binary!(lhs, ">", rhs),
                BinaryOp::Equal => binary!(lhs, "===", rhs),
                BinaryOp::Pow => {
                    js.write("Math.pow")?;
                    binary!(lhs, ",", rhs)
                }
                BinaryOp::CopySign => {
                    js.write("env.copySign")?;
                    binary!(lhs, ",", rhs)
                }
            },
        }

        Ok(())
    }
}
