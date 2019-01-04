use rustc_hash::FxHashMap;
use std::mem;
use void::Void;

use crate::ir::*;

#[derive(Copy, Clone)]
enum GlobalUsage {
    Single(usize),
    Multiple,
    None,
}

struct FunctionUpdater<'a> {
    function: &'a mut Function,
    init_statements: Vec<Statement>,
}
impl<'a> FunctionUpdater<'a> {
    fn replace_global(&mut self, g: GlobalKind) {
        let local_index = self.function.locals.len();
        self.function.locals.push(g.into());
        self.function.replace_global(g, local_index);

        self.init_statements.push(Statement::Assign(
            LValue::Local(local_index),
            Expr::Const(0.0),
        ));
    }
    fn done(mut self) {
        if !self.init_statements.is_empty() {
            let entry = self.function.entry;
            let entry_statements =
                &mut self.function.blocks.get_mut(entry).unwrap().statements;
            self.init_statements.append(entry_statements);
            *entry_statements = self.init_statements;
        }
    }
}

pub fn demote_global(ir: &mut Program) {
    let mut func_indices: FxHashMap<GlobalKind, GlobalUsage> = ir
        .globals
        .iter()
        .map(|g| (g.clone(), GlobalUsage::None))
        .collect();

    for g in &ir.globals {
        for (i, func) in ir.functions.iter_mut().enumerate() {
            if !func.contains_global(*g) {
                continue;
            }
            match func_indices.get_mut(g).unwrap_or(&mut GlobalUsage::None) {
                GlobalUsage::Multiple => {}
                u @ GlobalUsage::Single(_) => {
                    *u = GlobalUsage::Multiple;
                }
                GlobalUsage::None => {
                    func_indices.insert(*g, GlobalUsage::Single(i));
                }
            }
        }
    }
    let mut globals = vec![];
    let mut updaters: Vec<_> = ir
        .functions
        .iter_mut()
        .map(|f| FunctionUpdater {
            function: f,
            init_statements: Vec::new(),
        })
        .collect();
    for (g, usage) in func_indices.iter() {
        match usage {
            GlobalUsage::Single(index) => {
                updaters[*index].replace_global(g.clone());
            }
            GlobalUsage::Multiple | GlobalUsage::None => {
                globals.push(g.clone());
            }
        }
    }
    for updater in updaters {
        updater.done();
    }

    ir.globals = globals;
}

trait TraverseLValue {
    fn traverse<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut LValue) -> Option<T> + Clone;

    fn contains_global(&mut self, global: GlobalKind) -> bool {
        self.traverse(|lval| match (lval, global) {
            (LValue::Global(a), GlobalKind::Variable(b)) => {
                if *a == b {
                    Some(())
                } else {
                    None
                }
            }
            _ => None,
        })
        .is_some()
    }

    fn replace_global(&mut self, global: GlobalKind, local: usize) {
        self.traverse::<Void, _>(move |lval| match (&lval, global) {
            (LValue::Global(a), GlobalKind::Variable(b)) => {
                if *a == b {
                    *lval = LValue::Local(local)
                }
                None
            }
            _ => None,
        });
    }
}

impl TraverseLValue for LValue {
    fn traverse<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut LValue) -> Option<T> + Clone,
    {
        match self {
            LValue::ArrPtr(_, offset) => match offset {
                Offset::OneD(expr) => expr.traverse(f),
                Offset::TwoD(i, j) => {
                    i.traverse(f.clone()).or_else(|| j.traverse(f))
                }
            },
            _ => f(self),
        }
    }
}

impl<'a, I, V: TraverseLValue> TraverseLValue for I
where
    I: Iterator<Item = &'a mut V>,
    V: 'a,
{
    fn traverse<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut LValue) -> Option<T> + Clone,
    {
        self.map(|v| v.traverse(f.clone()))
            .find(Option::is_some) // short circuits
            .map(Option::unwrap)
    }
}

impl TraverseLValue for Expr {
    fn traverse<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut LValue) -> Option<T> + Clone,
    {
        match self {
            Expr::Get(ref mut lval) => f(lval),
            Expr::Unary(_, expr) => expr.traverse(f),
            Expr::Binary(_, ref mut lhs, ref mut rhs) => {
                lhs.traverse(f.clone()).or_else(|| rhs.traverse(f))
            }
            Expr::Call(ref mut lval, ref mut arg) => {
                lval.traverse(f.clone()).or_else(|| arg.traverse(f))
            }
            _ => None,
        }
    }
}

impl TraverseLValue for Statement {
    fn traverse<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut LValue) -> Option<T> + Clone,
    {
        match self {
            Statement::Assign(ref mut lval, ref mut expr) => {
                lval.traverse(f.clone()).or_else(|| expr.traverse(f))
            }
            Statement::DefFn(ref mut lval, _) => f(lval),
            Statement::Alloc1d(ref mut lval, ref mut expr) => {
                lval.traverse(f.clone()).or_else(|| expr.traverse(f))
            }
            Statement::Alloc2d(ref mut lval, ref mut r, ref mut c) => lval
                .traverse(f.clone())
                .or_else(|| r.traverse(f.clone()))
                .or_else(|| c.traverse(f)),
            Statement::Print(ref mut expr) => expr.traverse(f),
            _ => None,
        }
    }
}

impl TraverseLValue for BasicBlock {
    fn traverse<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut LValue) -> Option<T> + Clone,
    {
        if let Some(v) = self.statements.iter_mut().traverse(f.clone()) {
            return Some(v);
        }

        match self.exit {
            BlockExit::Return(Some(ref mut expr)) => expr.traverse(f),
            BlockExit::Switch(ref mut cond, ..) => cond.traverse(f),
            _ => None,
        }
    }
}

impl TraverseLValue for Function {
    fn traverse<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut LValue) -> Option<T> + Clone,
    {
        self.blocks.iter_mut().map(|(_, b)| b).traverse(f)
    }
}
