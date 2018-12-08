use either::Either;
use int_hash::{IntHashMap, IntHashSet};

use crate::ast::*;
use crate::ir::{
    Instruction, InstructionKind, Label, LabelIdGen, Visitor as IRVisitor,
};
use crate::vm::*;

use super::anon_var::AnonVarGen;
use super::array_dims::ArrayDims;
use super::data::PrepareData;
use super::error::CompileError as CompileErrorInner;
// use super::func_compiler::FuncCompiler;
use super::ir_labels::IrLabels;
use super::line_order::LineOrder;

#[derive(Debug)]
pub struct CompileError {
    pub inner: CompileErrorInner,
    pub line_no: usize,
}

struct CompileState {
    assign: bool,
    line: LineNo,
    local_pool: LocalVarPool,
    label_id_gen: LabelIdGen,
}

struct ForState {
    var: Variable,
    step: Variable,
    to: Variable,
    loop_start: Label,
    next_label: Label,
}

struct LocalVarPool {
    free: IntHashSet<Variable>,
    in_use: IntHashSet<Variable>,
    var_gen: AnonVarGen,
}
impl LocalVarPool {
    fn get_local(&mut self) -> Variable {
        let var = match self.free.iter().next() {
            Some(var) => *var,
            None => self.var_gen.next_anonymous(),
        };

        self.free.remove(&var);
        self.in_use.insert(var);

        var
    }
    fn mask_global(&mut self, var: Variable) {
        self.in_use.insert(var);
    }
    fn unmask_global(&mut self, var: Variable) -> Result {
        if self.in_use.remove(&var) {
            Ok(())
        } else {
            Err(CompileErrorInner::Custom("Global variable not in use"))
        }
    }
    fn is_in_use(&self, var: &Variable) -> bool {
        self.in_use.contains(var)
    }
    fn reclaim_local(&mut self, var: Variable) {
        if self.in_use.remove(&var) {
            self.free.insert(var);
        }
    }
}
// Scoping of mask_global is implemented as a macro.
//
// Implemeting a method on `LocalVarPool` to take a closure
// is obviously better, however, that requires self to be borrowed
// mutablely for the entire lifetime of the closure, which
// will block compiler method calls inside (all requires exclusive
// &mut self ref). This macro ensures make/unmask is always done
// in pairs. A similar macro is defined for function compile scope.
//
// The other solution is to store an `Option<LocalVarPool>` in `Compiler`,
// and use `take()` (or mem::replace), however, I think that
// adds a lot of noise and detracts from the main logic of underlying
// code.
macro_rules! mask_global {
    ($pool: expr, $var: expr, $body: expr) => {{
        $pool.mask_global($var);

        let r = $body;

        $pool.unmask_global($var)?;

        r
    }};
}

pub struct Target<T> {
    main: FuncId,
    current: FuncId,
    func_id_gen: FuncIdGen,
    functions: IntHashMap<FuncId, T>,
}
impl<T: IRVisitor + Default> Target<T>
where
    T::Error: Into<CompileErrorInner>,
{
    fn new_function(&mut self) -> FuncId {
        let func_id = self.func_id_gen.next_id();
        self.functions.insert(func_id, T::default());
        self.current = func_id;

        func_id
    }

    fn restore(&mut self, func_id: FuncId) -> Result {
        if self.current == func_id {
            self.current = self.main;

            Ok(())
        } else {
            Err(CompileErrorInner::Custom("Unenclosed function"))
        }
    }
}

macro_rules! function {
    ($tgt: expr, $body: expr) => {{
        let func_id = $tgt.new_function();

        $body?;

        $tgt.restore(func_id)?;

        func_id
    }};
}

impl<T: IRVisitor> IRVisitor for Target<T>
where
    T::Error: Into<CompileErrorInner>,
{
    type Output = (FuncId, IntHashMap<FuncId, T::Output>);
    type Error = CompileErrorInner;

    fn finish(mut self) -> ::std::result::Result<Self::Output, Self::Error> {
        let main = self.main;
        let n = self.functions.len();

        let outputs: IntHashMap<_, _> = self
            .functions
            .drain()
            .filter_map(|(func_id, t)| t.finish().map(|x| (func_id, x)).ok())
            .collect();

        if n == outputs.len() {
            Ok((main, outputs))
        } else {
            Err(CompileErrorInner::Custom("Compile failed"))
        }
    }

    #[inline]
    fn visit_instruction(
        &mut self,
        instr: Instruction,
    ) -> ::std::result::Result<(), Self::Error> {
        let current_visitor = self
            .functions
            .get_mut(&self.current)
            .ok_or(CompileErrorInner::Custom("Missing function"))?;

        current_visitor
            .visit_instruction(instr)
            .map_err(<T as IRVisitor>::Error::into)
    }
}

pub struct Compiler<V> {
    state: CompileState,
    for_states: IntHashMap<Variable, ForState>,
    label_mapping: IntHashMap<LineNo, Label>,
    ir_visitor: V,
}

type Result = ::std::result::Result<(), CompileErrorInner>;

impl<V: IRVisitor> Compiler<V>
where
    V::Error: Into<CompileErrorInner>,
{
    fn emit_instruction(&mut self, instr_kind: InstructionKind) -> Result {
        let line_no = self.state.line;
        let label = self.label_mapping.get(&line_no).cloned();

        let instr = Instruction {
            label,
            line_no,
            kind: instr_kind,
        };

        self.ir_visitor
            .visit_instruction(instr)
            .map_err(|e| e.into())
    }

    fn emit_labeled_instruction(
        &mut self,
        label: Label,
        instr_kind: InstructionKind,
    ) -> Result {
        let line_no = self.state.line;

        let instr = Instruction {
            label: Some(label),
            line_no,
            kind: instr_kind,
        };

        self.ir_visitor
            .visit_instruction(instr)
            .map_err(|e| e.into())
    }

    fn assigning<T, F>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let prev_assign = self.state.assign;
        self.state.assign = true;
        let ret = f(self);
        self.state.assign = prev_assign;
        ret
    }
    fn evaluating<T, F>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let prev_assign = self.state.assign;
        self.state.assign = false;
        let ret = f(self);
        self.state.assign = prev_assign;
        ret
    }
}
impl<V: IRVisitor + Default> Compiler<Target<V>>
where
    V::Error: Into<CompileErrorInner>,
{
    pub fn new() -> Self {
        let mut func_id_gen = FuncIdGen::new();
        let main_id = func_id_gen.next_id();

        let mut functions = IntHashMap::default();
        let main = V::default();

        functions.insert(main_id, main);

        Compiler {
            ir_visitor: Target {
                main: main_id,
                current: main_id,
                func_id_gen,
                functions,
            },
            state: CompileState {
                assign: false,
                line: 0,
                label_id_gen: LabelIdGen::new(),
                local_pool: LocalVarPool {
                    free: IntHashSet::default(),
                    in_use: IntHashSet::default(),
                    var_gen: AnonVarGen::new(),
                },
            },
            for_states: IntHashMap::default(),
            label_mapping: IntHashMap::default(),
        }
    }
    pub fn compile(
        mut self,
        prog: &Program,
    ) -> ::std::result::Result<
        (FuncId, IntHashMap<FuncId, V::Output>),
        CompileError,
    > {
        let line_no = self.state.line;

        self.visit_program(prog)
            .and_then(move |_| self.ir_visitor.finish())
            .map_err(|err| CompileError {
                inner: err,
                line_no,
            })
    }
}

impl<V: IRVisitor> Visitor<Result> for Compiler<Target<V>>
where
    V::Error: Into<CompileErrorInner>,
    V: Default,
{
    fn visit_program(&mut self, prog: &Program) -> Result {
        // check input line numbers are in order and unique
        let mut line_order = LineOrder::new();
        line_order.visit_program(prog)?;

        // preprocess: generate labels for certain line numbers
        let mut ir_labels = IrLabels::new(
            &mut self.state.label_id_gen,
            &mut self.label_mapping,
        );
        ir_labels.visit_program(prog)?;

        // preprocess: collect array dimension info for variables
        // and ensure the same symbol is not used as both 1d and 2d
        let mut array_dims = ArrayDims::new(&mut self.ir_visitor);
        array_dims.visit_program(prog)?;

        // preprocess: collect data statements
        let mut prep_data = PrepareData::new(&mut self.ir_visitor);
        prep_data.visit_program(prog)?;

        for s in &prog.statements {
            self.visit_statement(s)?;
        }

        // adds a stop instruction to last line (implicit END)
        self.emit_instruction(InstructionKind::Stop)
    }

    fn visit_statement(&mut self, stmt: &Statement) -> Result {
        let line_no = stmt.line_no;
        self.state.line = line_no;

        match &stmt.statement {
            Stmt::Let(s) => self.visit_let(s),
            Stmt::Read(s) => self.visit_read(s),
            Stmt::Data(s) => self.visit_data(s),
            Stmt::Print(s) => self.visit_print(s),
            Stmt::Goto(s) => self.visit_goto(s),
            Stmt::Gosub(s) => self.visit_gosub(s),
            Stmt::If(s) => self.visit_if(s),
            Stmt::For(s) => self.visit_for(s),
            Stmt::Next(s) => self.visit_next(s),
            Stmt::Def(s) => self.visit_def(s),
            Stmt::Dim(s) => self.visit_dim(s),
            Stmt::End => self.visit_end(),
            Stmt::Rem => self.visit_rem(),
            Stmt::Stop => self.visit_stop(),
            Stmt::Return => self.visit_return(),
        }
    }

    fn visit_let(&mut self, stmt: &LetStmt) -> Result {
        self.evaluating(|this| this.visit_expr(&stmt.expr))?;
        self.assigning(|this| this.visit_lvalue(&stmt.var))
    }

    fn visit_read(&mut self, stmt: &ReadStmt) -> Result {
        self.assigning(|this| {
            for var in &stmt.vars {
                match this.visit_lvalue(var) {
                    Ok(_) => {}
                    Err(e) => return Err(e),
                }
            }

            Ok(())
        })
    }

    fn visit_data(&mut self, _stmt: &DataStmt) -> Result {
        self.emit_instruction(InstructionKind::Noop)
    }

    fn visit_print(&mut self, stmt: &PrintStmt) -> Result {
        self.emit_instruction(InstructionKind::PrintStart)?;

        for part in &stmt.parts {
            match part {
                Printable::Expr(expr) => {
                    self.visit_expr(expr)?;
                    self.emit_instruction(InstructionKind::PrintExpr)?;
                }
                Printable::Advance3 => {
                    self.emit_instruction(InstructionKind::PrintAdvance3)?;
                }
                Printable::Advance15 => {
                    self.emit_instruction(InstructionKind::PrintAdvance15)?;
                }
                Printable::Label(s) => {
                    self.emit_instruction(InstructionKind::PrintLabel(
                        s.clone(),
                    ))?;
                }
            }
        }

        self.emit_instruction(InstructionKind::PrintEnd)
    }

    fn visit_goto(&mut self, stmt: &GotoStmt) -> Result {
        let label = self
            .label_mapping
            .get(&stmt.goto)
            .cloned()
            .ok_or(CompileErrorInner::Custom("Unexpected GOTO"))?;

        self.emit_instruction(InstructionKind::Jump(label))
    }

    fn visit_gosub(&mut self, stmt: &GosubStmt) -> Result {
        let label = self
            .label_mapping
            .get(&stmt.goto)
            .cloned()
            .ok_or(CompileErrorInner::Custom("Unexpected GOSUB"))?;

        self.emit_instruction(InstructionKind::Subroutine(label))
    }

    fn visit_if(&mut self, stmt: &IfStmt) -> Result {
        self.visit_expr(&stmt.lhs)?;
        self.visit_expr(&stmt.rhs)?;

        match stmt.op {
            Relop::Equal => {
                self.emit_instruction(InstructionKind::Equal)?;
            }
            Relop::NotEqual => {
                self.emit_instruction(InstructionKind::Equal)?;
                self.emit_instruction(InstructionKind::Not)?;
            }
            Relop::Less => {
                self.emit_instruction(InstructionKind::Less)?;
            }
            Relop::Greater => {
                self.emit_instruction(InstructionKind::Greater)?;
            }
            Relop::LessEqual => {
                self.emit_instruction(InstructionKind::Greater)?;
                self.emit_instruction(InstructionKind::Not)?;
            }
            Relop::GreaterEqual => {
                self.emit_instruction(InstructionKind::Less)?;
                self.emit_instruction(InstructionKind::Not)?;
            }
        }

        let label = self
            .label_mapping
            .get(&stmt.then)
            .cloned()
            .ok_or(CompileErrorInner::Custom("Unexpected THEN"))?;

        self.emit_instruction(InstructionKind::JumpTrue(label))
    }

    fn visit_for(&mut self, stmt: &ForStmt) -> Result {
        let step_var = self.state.local_pool.get_local();
        let to_var = self.state.local_pool.get_local();

        match stmt.step {
            Some(ref step_expr) => {
                self.visit_expr(step_expr)?;
            }
            None => {
                self.emit_instruction(InstructionKind::Constant(1.0))?;
            }
        }
        self.emit_instruction(InstructionKind::Dup)?;
        self.assigning(|this| this.visit_variable(&step_var))?;

        self.visit_expr(&stmt.from)?;
        self.emit_instruction(InstructionKind::Dup)?;
        self.assigning(|this| this.visit_variable(&stmt.var))?;

        self.visit_expr(&stmt.to)?;
        self.emit_instruction(InstructionKind::Dup)?;
        self.assigning(|this| this.visit_variable(&to_var))?;
        // value stack: [step, current, to]

        let start_label = self.state.label_id_gen.next_id();
        let next_label = self.state.label_id_gen.next_id();
        self.emit_instruction(InstructionKind::Jump(next_label))?;

        let for_state = ForState {
            var: stmt.var,
            step: step_var,
            to: to_var,
            loop_start: start_label,
            next_label: next_label,
        };

        self.emit_labeled_instruction(start_label, InstructionKind::Noop)?;

        self.for_states.insert(stmt.var, for_state);

        Ok(())
    }

    fn visit_next(&mut self, stmt: &NextStmt) -> Result {
        // Do not enforce nesting of for/next pairs, have
        // the runtime deal with it
        let for_state = self
            .for_states
            .remove(&stmt.var)
            .ok_or(CompileErrorInner::NextWithoutFor)?;

        let step_var = for_state.step;
        let to_var = for_state.to;
        let loop_start = for_state.loop_start;

        self.visit_variable(&step_var)?;

        self.emit_instruction(InstructionKind::Dup)?;
        self.visit_variable(&stmt.var)?;

        self.emit_instruction(InstructionKind::Add)?;
        self.emit_instruction(InstructionKind::Dup)?;

        self.assigning(|this| this.visit_variable(&stmt.var))?;
        self.visit_variable(&to_var)?;

        // value stack: [step, current, to]
        self.emit_labeled_instruction(
            for_state.next_label,
            InstructionKind::LoopTest,
        )?;
        self.emit_instruction(InstructionKind::JumpFalse(loop_start))?;

        self.state.local_pool.reclaim_local(step_var);
        self.state.local_pool.reclaim_local(to_var);

        Ok(())
    }

    fn visit_def(&mut self, stmt: &DefStmt) -> Result {
        let var = stmt.var;
        let func = stmt.func;
        let expr = &stmt.expr;

        mask_global!(self.state.local_pool, var, {
            let func_id = function!(self.ir_visitor, {
                self.emit_instruction(InstructionKind::DefineLocal(var))?;
                self.visit_expr(expr)?;
                self.emit_instruction(InstructionKind::Return)
            });
            self.emit_instruction(InstructionKind::MapFunc(func, func_id))
        })
    }

    fn visit_dim(&mut self, stmt: &DimStmt) -> Result {
        for dim in &stmt.dims {
            match dim {
                Either::Left(list) => {
                    self.visit_expr(&list.subscript)?;
                    self.emit_instruction(InstructionKind::SetArrayBound(
                        list.var,
                    ))?;
                }
                Either::Right(table) => {
                    self.visit_expr(&table.subscript.0)?;
                    self.visit_expr(&table.subscript.1)?;
                    self.emit_instruction(InstructionKind::SetArrayBound2d(
                        table.var,
                    ))?;
                }
            }
        }

        Ok(())
    }

    fn visit_rem(&mut self) -> Result {
        self.emit_instruction(InstructionKind::Noop)
    }

    fn visit_end(&mut self) -> Result {
        self.emit_instruction(InstructionKind::Stop)
    }

    fn visit_stop(&mut self) -> Result {
        self.emit_instruction(InstructionKind::Stop)
    }

    fn visit_return(&mut self) -> Result {
        self.emit_instruction(InstructionKind::Return)
    }

    fn visit_variable(&mut self, var: &Variable) -> Result {
        let is_local = self.state.local_pool.is_in_use(var);

        match (self.state.assign, is_local) {
            (true, true) => {
                self.emit_instruction(InstructionKind::SetLocal(*var))
            }
            (true, false) => {
                self.emit_instruction(InstructionKind::SetGlobal(*var))
            }
            (false, true) => {
                self.emit_instruction(InstructionKind::GetLocal(*var))
            }
            (false, false) => {
                self.emit_instruction(InstructionKind::GetGlobal(*var))
            }
        }
    }

    fn visit_list(&mut self, list: &List) -> Result {
        self.evaluating(|this| this.visit_expr(&list.subscript))?;

        if self.state.assign {
            self.emit_instruction(InstructionKind::SetGlobalArray(list.var))
        } else {
            self.emit_instruction(InstructionKind::GetGlobalArray(list.var))
        }
    }

    fn visit_table(&mut self, table: &Table) -> Result {
        self.evaluating(|this| {
            this.visit_expr(&table.subscript.0)
                .and_then(|_| this.visit_expr(&table.subscript.1))
        })?;

        if self.state.assign {
            self.emit_instruction(InstructionKind::SetGlobalArray2d(table.var))
        } else {
            self.emit_instruction(InstructionKind::GetGlobalArray2d(table.var))
        }
    }

    fn visit_expr(&mut self, expr: &Expression) -> Result {
        if self.state.assign {
            // this in theory should not happen... parser should've taken
            // care of it, if observed, probably a compiler bug
            return Err(CompileErrorInner::CannotAssignTo(expr.to_string()));
        }
        match expr {
            Expression::Lit(n) => {
                self.emit_instruction(InstructionKind::Constant(*n))
            }
            Expression::Var(v) => self.visit_lvalue(v),
            Expression::Call(func, arg) => {
                self.visit_expr(arg)?;

                if func.is_native() {
                    self.emit_instruction(InstructionKind::CallNative(*func))
                } else {
                    self.emit_instruction(InstructionKind::Call(*func))
                }
            }
            Expression::Neg(expr) => {
                self.visit_expr(expr)?;
                self.emit_instruction(InstructionKind::Negate)
            }
            Expression::Add(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.emit_instruction(InstructionKind::Add)
            }
            Expression::Sub(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.emit_instruction(InstructionKind::Sub)
            }
            Expression::Mul(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.emit_instruction(InstructionKind::Mul)
            }
            Expression::Div(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.emit_instruction(InstructionKind::Div)
            }
            Expression::Pow(lhs, rhs) => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
                self.emit_instruction(InstructionKind::Pow)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::*;

    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::vm::{Chunk, VM};

    #[derive(Default, Debug)]
    struct PrintIR;

    impl IRVisitor for PrintIR {
        type Output = ();
        type Error = CompileErrorInner;

        fn finish(self) -> Result {
            Ok(())
        }
        fn visit_instruction(&mut self, instr: Instruction) -> Result {
            println!("{:?}", instr);
            Ok(())
        }
    }

    #[test]
    fn test_simple() {
        let program = indoc!(
            "
            10 LET X = SIN(10)
            15 DIM X(20, 20), Y(15)
            20 READ X, Y, A
            21 FOR I = 0 TO 14
            22   LET Y(I) = I + 1000
            23 NEXT I
            24 FOR I = 10 TO 1 STEP -1
            25   PRINT Y(I)
            26 NEXT I
            31 DEF FNA(D) = D + 10
            32 DEF FNB(X) = X + FNA(X) + Y
            40 PRINT \"FNB(Y)\", FNB(Y)
            50 REM IGNORE ME
            55 LET X(3, 3) = 99
            60 GOSUB 100
            70 PRINT \"X(3, 3)\"; X(3, 3)
            80 GOTO 800
            100 REM SUBROUTING
            120 PRINT \"拼音\", A
            130 RETURN
            150 DATA 10, 20, 30
            800 END"
        );
        let printed = indoc!(
            "
            1010
            1009
            1008
            1007
            1006
            1005
            1004
            1003
            1002
            1001
            FNB(Y)         70
            拼音           30
            X(3, 3)  99
            "
        );

        let scanner = Scanner::new(program);
        let ast = Parser::new(scanner).parse().unwrap();

        let compiler: Compiler<Target<PrintIR>> = Compiler::new();
        let (_, fns) = compiler.compile(&ast).unwrap();

        println!("functions: {:?}", fns);

        assert!(false);

        // let mut vm = VM::new(chunk);
        // let mut output = Vec::new();
        // vm.run(&mut output);

        // assert_eq!(::std::str::from_utf8(&output), Ok(printed));
    }
}
