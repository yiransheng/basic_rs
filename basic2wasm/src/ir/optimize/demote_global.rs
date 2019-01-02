use std::mem;

use rustc_hash::FxHashMap;
use void::Void;

use crate::ir::*;

#[derive(Copy, Clone)]
enum GlobalUsage {
    Single(usize),
    Multiple,
}

pub fn demote_global(ir: &mut Program) {
    let mut func_indices: FxHashMap<GlobalKind, GlobalUsage> =
        FxHashMap::default();

    for g in &ir.globals {
        for (i, func) in ir.functions.iter_mut().enumerate() {
            if !func.contains_global(*g) {
                continue;
            }
            match func_indices.get_mut(g) {
                Some(GlobalUsage::Multiple) => {}
                Some(u @ GlobalUsage::Single(_)) => {
                    *u = GlobalUsage::Multiple;
                }
                None => {
                    func_indices.insert(*g, GlobalUsage::Single(i));
                }
            }
        }
    }
    let mut globals = vec![];
    for (g, usage) in func_indices.iter() {
        match usage {
            GlobalUsage::Single(index) => {
                let func = &mut ir.functions[*index];
                let local_index = func.locals.len();
                func.replace_global(g.clone(), local_index);
                func.locals.push(g.clone().into());
            }
            GlobalUsage::Multiple => {
                globals.push(g.clone());
            }
            _ => {}
        }
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
        f(self)
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
