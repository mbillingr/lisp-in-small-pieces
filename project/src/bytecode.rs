#[derive(Debug)]
pub enum Op {
    Constant(usize),
    JumpFalse(isize),
    Jump(isize),
}
