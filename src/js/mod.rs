use std::fmt;
use std::io::{self, Write};

use crate::ir::*;
use crate::relooper::{
    Cond, FlowType, LoopCtx, Relooper, Render, RenderSink, ShapeId,
};

struct JsCode<'a, W> {
    out: W,
    function: FunctionName,
    ir: &'a Program,
}
impl<'a, W> JsCode<'a, W> {
    fn function(&self) -> &'a Function {
        self.ir
            .functions
            .iter()
            .find(|func| func.name == self.function)
            .unwrap()
    }
}

impl<'a, W> RenderSink for JsCode<'a, W>
where
    W: Write,
{
    fn render_loop<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Self),
    {
        writeln!(&mut self.out, "while (1) {}", "{");

        f(self);

        writeln!(&mut self.out, "{}", "}");
    }

    fn render_condition<C: Render<Self>, F>(
        &mut self,
        ctx: LoopCtx,
        cond: Cond<&C>,
        mut f: F,
    ) where
        F: FnMut(&mut Self),
    {
        match cond {
            Cond::If(expr) => {
                write!(&mut self.out, "if (");
                expr.render(ctx, self);
                writeln!(&mut self.out, ") {}", "{");
                f(self);
                writeln!(&mut self.out, "{}", "}");
            }
            Cond::ElseIf(expr) => {
                write!(&mut self.out, " else if (");
                expr.render(ctx, self);
                writeln!(&mut self.out, ") {}", "{");
                f(self);
                writeln!(&mut self.out, "{}", "}");
            }
            _ => {}
        }
    }

    fn render_shape_id(&mut self, shape_id: ShapeId) {
        write!(&mut self.out, "$L{}", shape_id.0);
    }

    fn render_flow(
        &mut self,
        ctx: LoopCtx,
        flow_type: FlowType,
        shape_id: Option<ShapeId>,
    ) {
        if let LoopCtx::Outside = ctx {
            return;
        }
        match flow_type {
            FlowType::Break => {
                write!(&mut self.out, "break");
            }
            FlowType::Continue => {
                write!(&mut self.out, "continue");
            }
            FlowType::Direct => return,
        }
        if let Some(shape_id) = shape_id {
            write!(&mut self.out, " ");
            self.render_shape_id(shape_id);
        }

        writeln!(&mut self.out, ";");
    }
}

impl<'a, W: Write> Render<JsCode<'a, W>> for Label {
    fn render(&self, ctx: LoopCtx, sink: &mut JsCode<'a, W>) {
        let func = sink.function();
        let block = match func.blocks.get(*self) {
            Some(block) => block,
            _ => return,
        };
        for s in &block.statements {
            s.codegen(sink).expect("write error");
        }
    }
}

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

impl<'a, W, T> Render<JsCode<'a, W>> for &T
where
    W: Write,
    T: ToJs,
{
    fn render(&self, ctx: LoopCtx, sink: &mut JsCode<'a, W>) {
        self.codegen(sink);
    }
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

        let mut js = JsCode {
            out: &mut js_src,
            function: ir.main,
            ir: &ir,
        };

        let mut relooper: Relooper<Label, &'_ Expr> = Relooper::new();

        let mut mapping: HashMap<Label, NodeId> = HashMap::new();

        for block in js.function().iter() {
            let label = block.label;
            let node_id = relooper.add_block(label);
            mapping.insert(label, node_id);
        }

        for block in js.function().iter() {
            let label = block.label;
            let node_id = mapping.get(&label).cloned().unwrap();

            match &block.exit {
                BlockExit::Return(_) => {}
                BlockExit::Jump(label) => {
                    let to_id = mapping.get(&label).cloned().unwrap();
                    relooper.add_branch(node_id, to_id, None);
                }
                BlockExit::Switch(cond, true_br, None) => {
                    let to_id = mapping.get(&true_br).cloned().unwrap();
                    relooper.add_branch(node_id, to_id, Some(cond));
                }
                BlockExit::Switch(cond, true_br, Some(false_br)) => {
                    let to_id = mapping.get(&true_br).cloned().unwrap();
                    relooper.add_branch(node_id, to_id, Some(cond));

                    let to_id = mapping.get(&false_br).cloned().unwrap();
                    // TODO: this is wrong
                    relooper.add_branch(node_id, to_id, None);
                }
            }
        }

        relooper.render(
            mapping.get(&js.function().entry).cloned().unwrap(),
            &mut js,
        );

        let js_src = ::std::str::from_utf8(&js_src).unwrap();

        println!("{}", js_src);

        assert!(false);
    }
}