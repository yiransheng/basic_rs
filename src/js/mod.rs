use std::fmt;
use std::io::{self, Write};

use crate::ir::*;
use crate::relooper::{
    Cond, FlowType, LoopCtx, NodeId, ProcessedBranch, Relooper, Render,
    RenderSink, ShapeId,
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
    fn function_name_string(&self, name: FunctionName) -> String {
        if name == self.ir.main {
            return "main".to_string();
        }
        self.ir
            .functions
            .iter()
            .enumerate()
            .find_map(|(i, func)| {
                if func.name == name {
                    Some(format!("fn${}", i))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| "undefined".to_string())
    }
}

impl<'a, W> RenderSink for JsCode<'a, W>
where
    W: Write,
{
    fn render_loop<F>(&mut self, shape_id: Option<ShapeId>, mut f: F)
    where
        F: FnMut(&mut Self),
    {
        if let Some(id) = shape_id {
            self.render_shape_id(id);
            write!(&mut self.out, ": ");
        }
        writeln!(&mut self.out, "while (1) {}", "{");

        f(self);

        writeln!(&mut self.out, "{}", "}");
    }

    // fn render_multi_loop<F>(&mut self, mut f: F)
    // where
    // F: FnMut(&mut Self),
    // {
    // writeln!(&mut self.out, "do {}", "{");

    // f(self);

    // writeln!(&mut self.out, "{} while(0)", "}");
    // }

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
                write!(&mut self.out, "else if (");
                expr.render(ctx, self);
                writeln!(&mut self.out, ") {}", "{");
                f(self);
                writeln!(&mut self.out, "{}", "}");
            }
            Cond::IfLabel(id) => {
                writeln!(
                    &mut self.out,
                    "if (_label === {}) {}",
                    id.index(),
                    "{"
                );
                f(self);
                writeln!(&mut self.out, "{}", "}");
            }
            Cond::ElseIfLabel(id) => {
                writeln!(
                    &mut self.out,
                    "else if (_label === {}) {}",
                    id.index(),
                    "{"
                );
                f(self);
                writeln!(&mut self.out, "{}", "}");
            }
            Cond::Else => {
                writeln!(&mut self.out, " else {}", "{");
                f(self);
                writeln!(&mut self.out, "{}", "}");
            }
            _ => {}
        }
    }

    fn render_shape_id(&mut self, shape_id: ShapeId) {
        write!(&mut self.out, "$L{}", shape_id.0);
    }

    fn render_branch<E: Render<Self>>(
        &mut self,
        br: &ProcessedBranch<E>,
        // set_label: bool, for now alawys set label
    ) {
        match br.flow_type {
            FlowType::Direct => {
                writeln!(&mut self.out, "_label = {};", br.target.index());
                return;
            }
            FlowType::Break => {
                write!(&mut self.out, "break");
            }
            FlowType::Continue => {
                write!(&mut self.out, "continue");
            }
        }
        write!(&mut self.out, " ");
        self.render_shape_id(br.ancestor);
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

        match block.exit {
            BlockExit::Return(None) => {
                sink.writeln("return;");
            }
            BlockExit::Return(Some(ref expr)) => {
                sink.write("return ");
                expr.codegen(sink);
                sink.writeln(";");
            }
            _ => {}
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

impl<'a, W> Render<JsCode<'a, W>> for FunctionName
where
    W: Write,
{
    fn render(&self, ctx: LoopCtx, sink: &mut JsCode<'a, W>) {
        use std::collections::HashMap;

        if *self != sink.function {
            return;
        }
        let func = sink.function();

        let sig = match func.ty.arg {
            Some(_) => "function ($x0) {\n",
            // generator
            _ => "function* () {\n",
        };

        sink.write_group(sig, "}", |sink| {
            sink.writeln("var _label;");

            for (i, ty) in func.locals.iter().enumerate() {
                match ty {
                    ValueType::F64 => {
                        sink.writeln(format_args!("var $x{};", i));
                    }
                    _ => unimplemented!(),
                }
            }
            let mut relooper: Relooper<Label, &'_ Expr> = Relooper::new();

            let mut mapping: HashMap<Label, NodeId> = HashMap::new();

            for block in func.iter() {
                let label = block.label;
                let node_id = relooper.add_block(label);
                mapping.insert(label, node_id);
            }

            for block in func.iter() {
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
                        relooper.add_branch(node_id, to_id, None);
                    }
                }
            }

            relooper.render(mapping.get(&func.entry).cloned().unwrap(), sink);

            Ok(())
        });
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
            Statement::DefFn(lval, func) => {
                lval.codegen(js)?;
                js.write(" = ")?;
                let name = js.function_name_string(*func);
                js.write(name)?;
            }
            Statement::CallSub(name) => {
                let name = js.function_name_string(*name);
                js.write(format_args!("{}()", name))?;
            }
            Statement::Alloc1d(lval, ..) => {
                // do nothing
            }
            Statement::Alloc2d(lval, ..) => {
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
                    js.write(format_args!("env.getArray({}).index1d(", var))?;
                    i.codegen(js)?;
                    js.write(").value")?;
                }
                Offset::TwoD(i, j) => {
                    js.write(format_args!("env.getArray({}).index2d(", var))?;
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
            5  FOR J = 1 TO 10
            10 IF I > 0 THEN 40
            20 PRINT \"I <= 0\"
            30 GOTO 50
            40 PRINT \"I > 0\"
            50 NEXT J
            99 END"
        );
        let program = include_str!("../../sample_programs/pi.bas");
        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let mut ir = compile(&ast).expect("it should compile successfuly");
        ir.optimize();

        println!("{}", ir);

        let mut js_src: Vec<u8> = Vec::new();

        let mut js = JsCode {
            out: &mut js_src,
            function: ir.main,
            ir: &ir,
        };

        ir.main.render(LoopCtx::Outside, &mut js);

        let js_src = ::std::str::from_utf8(&js_src).unwrap();

        println!("{}", js_src);

        assert!(false);
    }
}
