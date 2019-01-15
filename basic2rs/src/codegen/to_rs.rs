use std::io::{self, Write};

use super::{RsCode, ToRs};
use basic_rs::ir::*;

impl ToRs for Statement {
    type Error = io::Error;

    fn codegen<'a, W: Write>(
        &'a self,
        rs: &mut RsCode<W>,
    ) -> Result<(), Self::Error> {
        match self {
            Statement::Assign(lval, expr) => {
                lval.codegen(rs)?;
                rs.write(" = ")?;
                expr.codegen(rs)?;
            }
            Statement::DefFn(lval, func) => {
                lval.codegen(rs)?;
                rs.write(" = ")?;
                let name = rs.function_name_string(*func);
                rs.write(name)?;
            }
            Statement::CallSub(name) => {
                let name = rs.function_name_string(*name);
                rs.write(format_args!("{}()", name))?;
            }
            Statement::Alloc1d(..) => {
                // do nothing
            }
            Statement::Alloc2d(..) => {
                // do nothing
            }
            Statement::Print(expr) => {
                rs.write_group("printer.write_num(", ")", |rs| {
                    expr.codegen(rs)
                })?;
            }
            Statement::PrintLabel(offset, len) => {
                let s = ::std::str::from_utf8(
                    &rs.ir.labels.as_bytes()[*offset..*offset + *len],
                )
                .unwrap();
                // not need to escape, BASIC source does not support escaping double quote, so any
                // string that needs escaping will fail at Parser
                rs.write_group("printer.write_str(\"", "\")", |rs| {
                    rs.write(s)
                })?;
            }
            Statement::PrintAdvance3 => {
                rs.write("printer.advance_to_multiple(3)")?;
            }
            Statement::PrintAdvance15 => {
                rs.write("printer.advance_to_multiple(15)")?;
            }
            Statement::PrintNewline => {
                rs.write("printer.writeln()")?;
            }
        }

        rs.writeln(";")?;

        Ok(())
    }
}

impl ToRs for GlobalKind {
    type Error = io::Error;

    fn codegen<'a, W: Write>(
        &'a self,
        rs: &mut RsCode<W>,
    ) -> Result<(), Self::Error> {
        match self {
            GlobalKind::Variable(var) => {
                rs.writeln(format_args!("let mut {} = 0;", var))
            }
            GlobalKind::ArrPtr(var, _) => rs.writeln(format_args!(
                "var ARRAY_{} = new SparseArray();",
                var
            )),
            GlobalKind::FnPtr(var) => {
                rs.writeln(format_args!("let mut {};", var))
            }
        }
    }
}

impl ToRs for LValue {
    type Error = io::Error;

    fn codegen<'a, W: Write>(
        &'a self,
        rs: &mut RsCode<W>,
    ) -> Result<(), Self::Error> {
        match self {
            LValue::ArrPtr(var, offset) => match offset {
                Offset::OneD(i) => {
                    rs.write(format_args!("ARRAY_{}.index1d(", var))?;
                    i.codegen(rs)?;
                    rs.write(").value")?;
                }
                Offset::TwoD(i, j) => {
                    rs.write(format_args!("ARRAY_{}.index2d(", var))?;
                    i.codegen(rs)?;
                    rs.write(", ")?;
                    j.codegen(rs)?;
                    rs.write(").value")?;
                }
            },
            LValue::FnPtr(func) => rs.write(func)?,
            LValue::Global(var) => rs.write(var)?,
            LValue::Local(index) => {
                rs.write(format_args!("local_{}", index))?
            }
        }

        Ok(())
    }
}

impl ToRs for Expr {
    type Error = io::Error;

    fn codegen<'a, W: Write>(
        &'a self,
        rs: &mut RsCode<W>,
    ) -> Result<(), Self::Error> {
        macro_rules! unary {
            ($op: expr, $operand: expr) => {{
                rs.write("( ")?;

                rs.write($op)?;
                $operand.codegen(rs)?;

                rs.write(" )")?;
            }};
        }

        macro_rules! call {
            ($fn: expr, $operand: expr) => {{
                rs.write($fn)?;
                rs.write("( ")?;

                $operand.codegen(rs)?;

                rs.write(" )")?;
            }};
        }

        macro_rules! call_method {
            ($fn: expr, $operand: expr) => {{
                $operand.codegen(rs)?;
                rs.write(".")?;
                rs.write($fn)?;
                rs.write("()")?;
            }};
        }

        macro_rules! binary {
            ($lhs: expr, $op: expr, $rhs: expr) => {{
                rs.write("( ")?;
                $lhs.codegen(rs)?;

                rs.write($op)?;

                $rhs.codegen(rs)?;
                rs.write(" )")?;
            }};
        }

        match self {
            Expr::RandF64 => rs.write("rng.gen()")?,
            Expr::ReadData => rs.write("data.pop().expect(\"no data\")")?,
            Expr::Input => rs.write("{ printer.flush(); env.input() }")?,
            Expr::Call(func, expr) => {
                func.codegen(rs)?;
                rs.write_group("(", ")", |rs| expr.codegen(rs))?;
            }
            Expr::Const(v) => rs.write(format_args!("{}f64", v))?,
            Expr::Get(lval) => lval.codegen(rs)?,
            Expr::Unary(op, operand) => match op {
                UnaryOp::Not => unary!("!", operand),
                UnaryOp::Neg => unary!("-", operand),
                UnaryOp::Sin => call_method!("sin", operand),
                UnaryOp::Cos => call_method!("cos", operand),
                UnaryOp::Atn => call_method!("atan", operand),
                UnaryOp::Exp => call_method!("exp", operand),
                UnaryOp::Abs => call_method!("abs", operand),
                UnaryOp::Log => call_method!("ln", operand),
                UnaryOp::Sqr => call_method!("sqrt", operand),
                UnaryOp::Trunc => call_method!("trunc", operand),
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
                    lhs.codegen(rs)?;
                    rs.write(".")?;
                    call!("powf", rhs)
                }
                BinaryOp::CopySign => {
                    unimplemented!();
                    rs.write("env.copySign")?;
                    binary!(lhs, ",", rhs)
                }
            },
        }

        Ok(())
    }
}
