use super::Instruction;

pub trait Visitor {
    type Output;
    type Error;

    fn finish(self) -> Result<Self::Output, Self::Error>;

    fn visit_instruction(
        &mut self,
        instr: Instruction,
    ) -> Result<(), Self::Error>;
}

impl<'a, V: Visitor> Visitor for &'a mut V {
    type Output = ();
    type Error = V::Error;

    fn finish(self) -> Result<Self::Output, Self::Error> {
        Ok(())
    }

    fn visit_instruction(
        &mut self,
        instr: Instruction,
    ) -> Result<(), Self::Error> {
        <V as Visitor>::visit_instruction(*self, instr)
    }
}
