mod to_js;

use std::fmt;
use std::io::{self, Write};

use basic_rs::ir::*;
use basic_rs::relooper::{
    Cond, FlowType, LoopCtx, NodeId, ProcessedBranch, Relooper, Render,
    RenderSink, ShapeId,
};

trait ToJs {
    type Error;

    fn codegen<'a, W: Write>(
        &'a self,
        js: &mut JsCode<W>,
    ) -> Result<(), Self::Error>;
}

pub fn generate_js<W: Write>(ir: &Program, out: W) {
    let mut js = JsCode {
        out,
        function: ir.main,
        ir,
    };

    for d in &ir.data {
        js.writeln_(format_args!("env.addData({});", d));
    }

    for global in &ir.globals {
        global.codegen(&mut js).unwrap();
    }

    for func in ir.functions.iter() {
        let name = func.name;
        js.function = name;
        js.write_(format_args!("var {} = ", js.function_name_string(name)));
        name.render(LoopCtx::Outside, &mut js);
        js.writeln_("");
    }

    js.write_("env.run(main());");
}

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

    fn write_<T: fmt::Display>(&mut self, x: T) {
        self.write(x).unwrap();
    }
    fn writeln_<T: fmt::Display>(&mut self, x: T) {
        self.writeln(x).unwrap();
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
            self.write_(": ");
        }
        self.writeln_("while (1) {");

        f(self);

        self.writeln_("}");
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
                self.write_("if (");
                expr.render(ctx, self);
                self.writeln_(") {");
                f(self);
                self.writeln_("}");
            }
            Cond::ElseIf(expr) => {
                self.write_("else if (");
                expr.render(ctx, self);
                self.writeln_(") {");
                f(self);
                self.writeln_("}");
            }
            Cond::IfLabel(id) => {
                self.writeln_(format_args!(
                    "if (_label === {}) {}",
                    id.index(),
                    "{"
                ));
                f(self);
                self.writeln_("}");
            }
            Cond::ElseIfLabel(id) => {
                self.writeln_(format_args!(
                    "else if (_label === {}) {}",
                    id.index(),
                    "{"
                ));
                f(self);
                self.writeln_("}");
            }
            Cond::Else => {
                self.writeln_("else {");
                f(self);
                self.writeln_("}");
            }
        }
    }

    fn render_shape_id(&mut self, shape_id: ShapeId) {
        self.write_(format_args!("$L{}", shape_id.0));
    }

    fn render_trap(&mut self) {
        self.writeln_("throw Error(\"Unreachable\");");
    }

    fn render_branch<E: Render<Self>>(&mut self, br: &ProcessedBranch<E>) {
        match br.flow_type {
            FlowType::Direct => {
                self.writeln_(format_args!("_label = {};", br.target.index()));
                return;
            }
            FlowType::Break => {
                self.write_(format_args!("break"));
            }
            FlowType::Continue => {
                self.write_(format_args!("continue"));
            }
        }

        self.write_(" ");
        self.render_shape_id(br.ancestor);
        self.writeln_(";");
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
                sink.writeln_("return;");
            }
            BlockExit::Return(Some(ref expr)) => {
                sink.write_("return ");
                expr.codegen(sink).unwrap();
                sink.writeln_(";");
            }
            _ => {}
        }
    }
}

impl<'a, W> Render<JsCode<'a, W>> for &Expr
where
    W: Write,
{
    fn render(&self, ctx: LoopCtx, sink: &mut JsCode<'a, W>) {
        self.codegen(sink).unwrap();
    }
}

impl<'a, W> Render<JsCode<'a, W>> for &Statement
where
    W: Write,
{
    fn render(&self, ctx: LoopCtx, sink: &mut JsCode<'a, W>) {
        self.codegen(sink).unwrap();
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
            sink.writeln_("var _label;");

            for (i, ty) in func.locals.iter().enumerate() {
                match ty {
                    ValueType::F64 => {
                        sink.writeln_(format_args!("var $x{};", i));
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
        })
        .unwrap();
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
        // let program = indoc!(
        // "
        // 5  FOR J = 1 TO 10
        // 10 IF I > 0 THEN 40
        // 20 PRINT \"I <= 0\"
        // 30 GOTO 50
        // 40 PRINT \"I > 0\"
        // 50 NEXT J
        // 99 END"
        // );
        // let program = include_str!("../../input.bas");
        // let scanner = Scanner::new(program);
        // let ast = Parser::new(scanner).parse().unwrap();

        // let mut ir = compile(&ast).expect("it should compile successfuly");
        // ir.optimize();

        // println!("{}", ir);

        // let mut js_src: Vec<u8> = Vec::new();

        // codegen_js(&ir, &mut js_src);

        // let js_src = ::std::str::from_utf8(&js_src).unwrap();

        // println!("{}", js_src);

        assert!(true);
    }
}
