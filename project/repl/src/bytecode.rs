use crate::interpreter::CodeObject;

#[derive(Copy, Clone, PartialEq)]
pub enum Op {
    Constant(usize),
    LocalRef(usize),
    FreeRef(usize),
    GlobalRef(usize),
    GlobalSet(usize),
    GlobalDef(usize),

    Boxify(usize),
    BoxSet,
    BoxGet,

    PushVal,

    JumpFalse(isize),
    Jump(isize),

    MakeClosure(usize, &'static CodeObject),
    PushCC(isize),
    PushEP(isize),
    PopEP,

    Call(usize),
    TailCall(usize),
    CallN,
    TailCallN,
    Return,
    Halt,
    PreApply(usize),

    Drop(usize),

    InitLibrary,

    // Intrinsics
    Cons,
    Car,
    Cdr,

    // Stack Operations
    Nip,
}

impl Op {
    pub fn is_terminal(&self) -> bool {
        match self {
            Op::Jump(_) | Op::TailCall(_) | Op::Return | Op::Halt => true,
            _ => false,
        }
    }
}
