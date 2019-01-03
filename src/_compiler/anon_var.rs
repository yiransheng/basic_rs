use crate::ast::variable::Variable;

pub struct AnonVarGen {
    id: u16,
}

impl AnonVarGen {
    pub fn new() -> Self {
        AnonVarGen { id: 0 }
    }
    pub fn next_anonymous(&mut self) -> Variable {
        // skip named variables (reserved for program use)
        while let Ok(_) = Variable::from_u16(self.id) {
            self.id += 1;
        }

        let var = Variable::from_u16_unchecked(self.id);
        self.id += 1;
        var
    }
}
