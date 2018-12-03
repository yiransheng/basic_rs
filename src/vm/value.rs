use nanbox::*;

use crate::ast::Variable;

make_nanbox! {
    #[derive(Copy, Clone)]
    pub unsafe enum Static, Variant {
        Number(f64),
        Label(u8),
        Varname([u8; 2])
    }
}

impl From<Variable> for Variant {
    fn from(var: Variable) -> Self {
        Variant::Varname(var.raw())
    }
}
