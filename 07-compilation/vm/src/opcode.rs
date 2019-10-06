#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Op {
    Nop = 0,
    ShallowArgumentRef0 = 1,
    ShallowArgumentRef1 = 2,
    ShallowArgumentRef2 = 3,
    ShallowArgumentRef3 = 4,
    ShallowArgumentRef = 5,
    DeepArgumentRef = 6,
    GlobalRef = 7,
    CheckedGlobalRef = 8,
    Constant = 9,
    Predefined0 = 10,
    Predefined1 = 11,
    Predefined2 = 12,
    Predefined3 = 13,
    Predefined4 = 14,
    Predefined5 = 15,
    Predefined6 = 16,
    Predefined7 = 17,
    Predefined8 = 18,
    Predefined = 19,
    Finish = 20,
    SetShallowArgument0 = 21,
    SetShallowArgument1 = 22,
    SetShallowArgument2 = 23,
    SetShallowArgument3 = 24,
    SetShallowArgument = 25,
    SetDeepArgument = 26,
    SetGlobal = 27,
    LongGoto = 28,
    LongJumpFalse = 29,
    ShortGoto = 30,
    ShortJumpFalse = 31,
    ExtendEnv = 32,
    UnlinkEnv = 33,
    PushValue = 34,
    PopArg1 = 35,
    PopArg2 = 36,
    PreserveEnv = 37,
    RestoreEnv = 38,
    PopFunction = 39,
    CreateClosure = 40,

    Return = 43,
    PackFrame = 44,
    FunctionInvoke = 45,
    FunctionGoto = 46,
    PopConsFrame = 47,

    AllocateFrame1 = 50,
    AllocateFrame2 = 51,
    AllocateFrame3 = 52,
    AllocateFrame4 = 53,
    AllocateFrame5 = 54,
    AllocateFrame = 55,
    AllocateDottedFrame = 56,

    PopFrame0 = 60,
    PopFrame1 = 61,
    PopFrame2 = 62,
    PopFrame3 = 63,
    PopFrame = 64,

    IsArity1 = 71,
    IsArity2 = 72,
    IsArity3 = 73,
    IsArity4 = 74,
    IsArity = 75,

    IsArityGreater = 78,
    ShortNumber = 79,
    ConstMinus1 = 80,
    Const0 = 81,
    Const1 = 82,
    Const2 = 83,
    Const4 = 84,

    Call1Car = 90,
    Call1Cdr = 91,
    Call1Pair = 92,
    Call1Symbol = 93,
    Call1Display = 94,
    Call1Null = 95,

    Call2Cons = 100,
    Call2Eq = 101,
    Call2SetCar = 102,
    Call2SetCdr = 103,
    Call2NumEq = 104,
    Call2Less = 105,
    Call2LessEq = 106,
    Call2Greater = 107,
    Call2GreaterEq = 108,
    Call2Add = 109,
    Call2Sub = 110,
    Call2Mul = 111,
    Call2Div = 112,

    DynamicRef = 240,
    DynamicPop = 241,
    DynamicPush = 242,

    NonContErr = 245,
    PushHandler = 246,
    PopHandler = 247,

    PushEscaper = 251,
}

impl Op {
    pub fn from_u8(b: u8) -> Self {
        unimplemented!()
    }

    pub unsafe fn from_u8_unchecked(b: u8) -> Self {
        std::mem::transmute(b)
    }
}
