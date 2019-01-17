mod to_rs;

use std::fmt;
use std::io::{self, Write};

use basic_rs::ir::*;
use basic_rs::relooper::{
    Cond, FlowType, LoopCtx, NodeId, ProcessedBranch, Relooper, Render,
    RenderSink, ShapeId,
};

trait ToRs {
    type Error;

    fn codegen<'a, W: Write>(
        &'a self,
        rs: &mut RsCode<W>,
    ) -> Result<(), Self::Error>;
}

pub fn generate_rs<W: Write>(ir: &Program, out: W) {
    let mut rs = RsCode {
        out,
        function: ir.main,
        ir,
    };

    rs.writeln_("#[derive(Debug, Default)]");
    rs.writeln_("struct Env {");

    for global in &ir.globals {
        global.codegen(&mut rs).unwrap();
    }

    rs.writeln_("}");

    rs.writeln_("fn main() {");
    rs.writeln_("use std::io;");
    rs.writeln_(
        "
        use rand::Rng;
        use rand::rngs::SmallRng;
        use rand::FromEntropy;",
    );

    rs.writeln_(
        "
        let mut env = Env::default();
        let stdout = io::stdout();
        let stdin = io::stdin();
        let mut rng = SmallRng::from_entropy();
    ",
    );
    rs.writeln_("let mut printer = Printer::new_buffered(stdout.lock());");

    rs.writeln_("let mut data: Vec<f64> = vec![");
    for d in &ir.data {
        rs.writeln_(format_args!("{}f64, ", d));
    }
    rs.writeln_("];");

    for func in ir.functions.iter() {
        let name = func.name;
        rs.function = name;
        rs.write_(format_args!("let mut {} = ", rs.function_name_string(name)));
        name.render(LoopCtx::Outside, &mut rs);
        rs.writeln_(";");
    }

    rs.writeln_("__main__(&mut env);");

    rs.writeln_("}");
}

struct RsCode<'a, W> {
    out: W,
    function: FunctionName,
    ir: &'a Program,
}

impl<'a, W> RsCode<'a, W> {
    fn function(&self) -> &'a Function {
        self.ir
            .functions
            .iter()
            .find(|func| func.name == self.function)
            .unwrap()
    }
    fn function_name_string(&self, name: FunctionName) -> String {
        if name == self.ir.main {
            return "__main__".to_string();
        }
        self.ir
            .functions
            .iter()
            .enumerate()
            .find_map(|(i, func)| {
                if func.name == name {
                    Some(format!("__fn_{}__", i))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| "undefined".to_string())
    }
}

impl<'a, W: Write> RsCode<'a, W> {
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

impl<'a, W> RenderSink for RsCode<'a, W>
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
        self.writeln_("loop {");

        f(self);

        self.writeln_("}");
    }

    fn render_multi_loop<F>(&mut self, shape_id: Option<ShapeId>, mut f: F)
    where
        F: FnMut(&mut Self),
    {
        if let Some(id) = shape_id {
            self.render_shape_id(id);
            self.write_(": ");
        }
        self.writeln_("loop {{");

        f(self);

        self.writeln_("} break;");
        self.writeln_("}");
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
                    "if (__label__ == {}) {}",
                    id.index(),
                    "{"
                ));
                f(self);
                self.writeln_("}");
            }
            Cond::ElseIfLabel(id) => {
                self.writeln_(format_args!(
                    "else if (__label__ == {}) {}",
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
        self.write_(format_args!("'a{}", shape_id.0));
    }

    fn render_trap(&mut self) {
        self.writeln_("unreachable!()");
    }

    fn render_branch<E: Render<Self>>(&mut self, br: &ProcessedBranch<E>) {
        self.writeln_(format_args!("__label__ = {};", br.target.index()));

        match br.flow_type {
            FlowType::Direct => {
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

impl<'a, W: Write> Render<RsCode<'a, W>> for Label {
    fn render(&self, _ctx: LoopCtx, sink: &mut RsCode<'a, W>) {
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

impl<'a, W> Render<RsCode<'a, W>> for &Expr
where
    W: Write,
{
    fn render(&self, _ctx: LoopCtx, sink: &mut RsCode<'a, W>) {
        self.codegen(sink).unwrap();
    }
}

impl<'a, W> Render<RsCode<'a, W>> for &Statement
where
    W: Write,
{
    fn render(&self, _ctx: LoopCtx, sink: &mut RsCode<'a, W>) {
        self.codegen(sink).unwrap();
    }
}

impl<'a, W> Render<RsCode<'a, W>> for FunctionName
where
    W: Write,
{
    fn render(&self, _ctx: LoopCtx, sink: &mut RsCode<'a, W>) {
        use std::collections::HashMap;

        if *self != sink.function {
            return;
        }
        let func = sink.function();

        let sig = match func.ty.arg {
            // TODO: not hard code type
            Some(_) => "|local_0: f64| {\n",
            // generator
            _ => "|env: &mut Env| {\n",
        };

        sink.write_group(sig, "}", |sink| {
            sink.writeln_("let mut __label__: usize;");

            for (i, ty) in func.locals.iter().enumerate() {
                match ty {
                    ValueType::F64 => {
                        sink.writeln_(format_args!(
                            "let mut local_{}: f64;",
                            i
                        ));
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
