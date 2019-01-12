use std::fmt;
use std::io::{self, Write};

use crate::ir::*;
use crate::relooper::Relooper;

struct JsCode<'a, W> {
    out: W,
    ir: &'a Program,
}

struct JsFn<'a, W> {
    function: &'a Function,
    js: JsCode<'a, W>,
}

// impl<'a, W: Write> Render for JsFn<'a, W> {
// type Base = Label;

// fn render_simple(&mut self, base: &Self::Base) {
// let block = self.function.blocks.get(*base).unwrap();

// for s in &block.statements {
// s.codegen(&mut self.js);
// }
// }

// fn render_loop<F>(&mut self, mut f: F)
// where
// F: FnMut(&mut Self),
// {
// self.js.writeln("while (true) {");

// f(self);

// self.js.writeln("}");
// }

// fn render_multi<F>(&mut self, mut f: F)
// where
// F: FnMut(&mut Self),
// {
// self.js.writeln("switch (true) {");

// f(self);

// self.js.writeln("}");
// }
// }

impl<'a, W: Write> JsCode<'a, W> {
    fn write_group<T1: fmt::Display, T2: fmt::Display, F>(
        &mut self,
        open: T1,
        close: T2,
        f: F,
    ) -> Result<(), io::Error>
    where
        F: Fn(&mut Self) -> Result<(), io::Error>,
    {
        write!(&mut self.out, "{}", open)?;

        f(self)?;

        write!(&mut self.out, "{}", close)?;

        Ok(())
    }
    fn write<T: fmt::Display>(&mut self, x: T) -> Result<(), io::Error> {
        write!(&mut self.out, "{}", x)
    }
    fn writeln<T: fmt::Display>(&mut self, x: T) -> Result<(), io::Error> {
        writeln!(&mut self.out, "{}", x)
    }
}

trait ToJs {
    type Error;

    fn codegen<'a, W: Write>(
        &'a self,
        js: &mut JsCode<W>,
    ) -> Result<(), Self::Error>;
}

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
            Statement::DefFn(..) => unimplemented!(),
            Statement::CallSub(..) => unimplemented!(),
            Statement::Alloc1d(..) => unimplemented!(),
            Statement::Alloc2d(..) => unimplemented!(),
            Statement::Print(expr) => {
                js.write_group("console.log(", ")", |js| expr.codegen(js))?;
            }

            Statement::PrintLabel(offset, len) => {
                let s = ::std::str::from_utf8(
                    &js.ir.labels.as_bytes()[*offset..*offset + *len],
                )
                .unwrap();
                js.write_group("console.log(", ")", |js| js.write(s))?;
            }
            _ => {}
        }

        js.writeln(";")?;

        Ok(())
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
                    js.write(var)?;
                    js.write_group("[", "]", |js| i.codegen(js))?;
                }
                Offset::TwoD(i, j) => {
                    js.write(var)?;

                    js.write_group("[", "]", |js| i.codegen(js))?;
                    js.write_group("[", "]", |js| j.codegen(js))?;
                }
            },
            LValue::FnPtr(func) => js.write(func)?,
            LValue::Global(var) => js.write(var)?,
            LValue::Local(index) => js.write(format_args!("x_{}", index))?,
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
            Expr::ReadData => unimplemented!(),
            Expr::Input => unimplemented!(),
            Expr::Call(..) => unimplemented!(),
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
                BinaryOp::CopySign => unimplemented!(),
            },
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::*;
    use std::collections::HashMap;

    use crate::compile::compile;
    use crate::parser::Parser;
    use crate::relooper::NodeId;
    use crate::scanner::Scanner;

    #[test]
    fn test_it() {
        let program = indoc!(
            "
            10 FOR I = 1 TO 10
            20 PRINT I
            30 NEXT I
            99 END"
        );
        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let ir = compile(&ast).expect("it should compile successfuly");

        let mut js_src: Vec<u8> = Vec::new();

        let js = JsCode {
            out: &mut js_src,
            ir: &ir,
        };

        let mut js_fn = JsFn {
            function: &ir.functions[0],
            js,
        };

        let mut relooper: Relooper<Label, Option<bool>> = Relooper::new();

        let mut mapping: HashMap<Label, NodeId> = HashMap::new();

        for block in js_fn.function.iter() {
            let label = block.label;
            let node_id = relooper.add_block(label);
            mapping.insert(label, node_id);
        }

        for block in js_fn.function.iter() {
            let label = block.label;
            let node_id = mapping.get(&label).cloned().unwrap();

            match &block.exit {
                BlockExit::Return(_) => {}
                BlockExit::Jump(label) => {
                    let to_id = mapping.get(label).cloned().unwrap();
                    relooper.add_branch(node_id, to_id, None);
                }
                BlockExit::Switch(_, true_br, None) => {
                    let to_id = mapping.get(true_br).cloned().unwrap();
                    relooper.add_branch(node_id, to_id, Some(true));
                }
                BlockExit::Switch(_, true_br, Some(false_br)) => {
                    let to_id = mapping.get(true_br).cloned().unwrap();
                    relooper.add_branch(node_id, to_id, Some(true));

                    let to_id = mapping.get(false_br).cloned().unwrap();
                    relooper.add_branch(node_id, to_id, Some(false));
                }
            }
        }

        relooper.render(
            mapping.get(&js_fn.function.entry).cloned().unwrap(),
            &mut js_fn,
        );

        let js_src = ::std::str::from_utf8(&js_src).unwrap();

        println!("{}", js_src);

        assert!(false);
    }
}
