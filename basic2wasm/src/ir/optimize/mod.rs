mod demote_global;

use self::demote_global::demote_global;
use super::Program;

pub fn optimize(ir: &mut Program) {
    demote_global(ir);
}
