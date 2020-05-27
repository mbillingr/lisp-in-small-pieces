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

    MakeClosure(usize),
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

impl std::fmt::Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Op::Constant(c) => write!(f, "(constant {})", c),
            Op::LocalRef(r) => write!(f, "(local-ref {})", r),
            Op::FreeRef(r) => write!(f, "(free-ref {})", r),
            Op::GlobalRef(r) => write!(f, "(global-ref {})", r),
            Op::GlobalSet(r) => write!(f, "(global-set {})", r),
            Op::GlobalDef(r) => write!(f, "(global-def {})", r),
            Op::Boxify(r) => write!(f, "(boxify {})", r),
            Op::BoxSet => write!(f, "(box-set)"),
            Op::BoxGet => write!(f, "(box-get)"),
            Op::PushVal => write!(f, "(push-val)"),
            Op::JumpFalse(loc) => write!(f, "(jump-false {})", loc),
            Op::Jump(loc) => write!(f, "(jump {})", loc),
            Op::MakeClosure(n_free) => write!(f, "(make-closure {})", n_free),
            Op::PushCC(o) => write!(f, "(push/cc {})", o),
            Op::PushEP(o) => write!(f, "(push/ep {})", o),
            Op::PopEP => write!(f, "(pop/ep)"),
            Op::Call(n_args) => write!(f, "(call {})", n_args),
            Op::TailCall(n_args) => write!(f, "(tail-call {})", n_args),
            Op::CallN => write!(f, "(call-n)"),
            Op::TailCallN => write!(f, "(tail-call-n)"),
            Op::Return => write!(f, "(return)"),
            Op::Halt => write!(f, "(halt)"),
            Op::PreApply(n_args) => write!(f, "(prepare-apply {})", n_args),
            Op::Drop(n) => write!(f, "(drop {})", n),
            Op::InitLibrary => write!(f, "(init-library)"),
            Op::Cons => write!(f, "(cons)"),
            Op::Car => write!(f, "(car)"),
            Op::Cdr => write!(f, "(cdr)"),
            Op::Nip => write!(f, "(nip)"),
        }
    }
}
