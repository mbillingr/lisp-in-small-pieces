use super::{
    ActivationFrame, Closure, CodePointer, Escape, OpaqueCast, OpaquePointer, Primitive,
    ScmBoxedValue,
};
use crate::memory::{PAIR_ALLOCATOR, VALUE_ALLOCATOR};
use lisp_core::lexpr;

//pub const DYNENV_TAG: Value = Value::Symbol("*dynenv*");
pub const DYNENV_TAG: Scm = Scm { ptr: 123 * 4 };
pub const ESCAPE_TAG: Scm = Scm { ptr: 456 * 4 };

pub const N_TAG_BITS: isize = 2;
pub const TAG_MASK: isize = 0b_11;
pub const TAG_POINTER: isize = 0b_00;
pub const TAG_INTEGER: isize = 0b_01;
pub const TAG_PAIR: isize = 0b_10;
//const TAG_SPECIAL: usize = 0b_11;

pub const SPECIAL_UNINIT: isize = 0b_0000_0111;
pub const SPECIAL_NULL: isize = 0b_0001_0111;
pub const SPECIAL_FALSE: isize = 0b_0010_0111;
pub const SPECIAL_TRUE: isize = 0b_0011_0111;

pub const SCM_MAX_INT: i64 = i64::max_value() / 4;
pub const SCM_MIN_INT: i64 = i64::min_value() / 4;

//const MASK_IMMEDIATE: usize = 0b01; // this works because all immediates have 1 in the lsb

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Scm {
    pub(crate) ptr: isize,
}

impl Scm {
    /*pub fn from_static_value(value: &'static Value) -> Self {
        Scm {
            ptr: ref_to_int(value),
        }
    }*/

    pub fn from_value(value: ScmBoxedValue) -> Self {
        //let p = Box::leak(Box::new(value));
        let p = VALUE_ALLOCATOR.with(|pa| pa.alloc(value));
        Scm { ptr: ref_to_int(p) }
    }

    pub fn null() -> Self {
        Scm { ptr: SPECIAL_NULL }
    }

    pub const fn uninitialized() -> Self {
        Scm {
            ptr: SPECIAL_UNINIT,
        }
    }

    pub fn bool(b: bool) -> Self {
        Scm {
            ptr: match b {
                true => SPECIAL_TRUE,
                false => SPECIAL_FALSE,
            },
        }
    }

    pub const fn int(i: i64) -> Self {
        Scm {
            ptr: (i as isize) << N_TAG_BITS | TAG_INTEGER,
        }
    }

    pub fn cons(car: Scm, cdr: Scm) -> Self {
        //let r = Box::leak(Box::new((car, cdr)));
        let r = PAIR_ALLOCATOR.with(|pa| pa.alloc((car, cdr)));
        let addr = ref_to_int(r);
        debug_assert!(addr & TAG_MASK == 0);
        Scm {
            ptr: addr + TAG_PAIR,
        }
    }

    pub fn symbol(s: &str) -> Self {
        let s = Box::leak(Box::new(s.to_owned()));
        Scm::from_value(ScmBoxedValue::Symbol(s))
    }

    pub fn string<T: ToString>(s: T) -> Self {
        let s = Box::leak(Box::new(s.to_string()));
        Scm::from_value(ScmBoxedValue::String(s))
    }

    pub fn primitive(p: Primitive) -> Self {
        let p = Box::leak(Box::new(p));
        Scm::from_value(ScmBoxedValue::Primitive(p))
    }

    pub fn frame(size: usize) -> Self {
        let frm = ActivationFrame::allocate(size);
        Scm::from_value(ScmBoxedValue::Frame(frm))
    }

    pub fn closure(code: CodePointer, env: &'static ActivationFrame) -> Self {
        let cls = Closure::allocate(code, env);
        Self::from_value(ScmBoxedValue::Closure(cls))
    }

    pub fn escape(stack_index: usize) -> Self {
        let esc = Escape::allocate(stack_index);
        Self::from_value(ScmBoxedValue::Escape(esc))
    }

    /*pub fn pointer(ptr: &'static u8) -> Self {
        Self::from_value(Value::Pointer(ptr))
    }*/

    fn as_ptr(&self) -> *const ScmBoxedValue {
        self.ptr as *const ScmBoxedValue
    }

    pub unsafe fn deref(&self) -> &ScmBoxedValue {
        &*self.as_ptr()
    }

    /*fn is_immediate(&self) -> bool {
        self.ptr & MASK_IMMEDIATE != 0
    }*/

    pub fn is_false(&self) -> bool {
        self.ptr == SPECIAL_FALSE
    }

    /*pub fn is_true(&self) -> bool {
        self.ptr == SPECIAL_TRUE
    }*/

    pub fn is_null(&self) -> bool {
        self.ptr == SPECIAL_NULL
    }

    pub fn is_uninitialized(&self) -> bool {
        self.ptr == SPECIAL_UNINIT
    }

    pub fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }

    pub fn is_symbol(&self) -> bool {
        self.as_symbol().is_some()
    }

    pub fn is_closure(&self) -> bool {
        self.as_closure().is_some()
    }

    pub fn is_escape(&self) -> bool {
        self.as_escape().is_some()
    }

    pub fn as_bool(&self) -> bool {
        match self.ptr {
            SPECIAL_FALSE => false,
            _ => true,
        }
    }

    pub fn as_value(&self) -> Option<&ScmBoxedValue> {
        if self.ptr & TAG_MASK == TAG_POINTER {
            Some(unsafe { self.deref() })
        } else {
            None
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        if self.ptr & TAG_MASK == TAG_INTEGER {
            Some((self.ptr >> N_TAG_BITS) as i64)
        } else {
            None
        }
    }

    pub fn as_pair(&self) -> Option<&(Scm, Scm)> {
        if self.ptr & TAG_MASK == TAG_PAIR {
            unsafe { Some(int_to_ref(self.ptr - TAG_PAIR)) }
        } else {
            None
        }
    }

    pub fn as_symbol(&self) -> Option<&'static str> {
        if self.ptr & TAG_MASK == TAG_POINTER {
            unsafe { self.deref().as_symbol() }
        } else {
            None
        }
    }

    pub fn as_primitive(&self) -> Option<&'static Primitive> {
        if self.ptr & TAG_MASK == TAG_POINTER {
            unsafe { self.deref().as_primitive() }
        } else {
            None
        }
    }

    pub fn as_frame(&self) -> Option<&'static ActivationFrame> {
        if self.ptr & TAG_MASK == TAG_POINTER {
            unsafe { self.deref().as_frame() }
        } else {
            None
        }
    }

    pub fn as_closure(&self) -> Option<&'static Closure> {
        if self.ptr & TAG_MASK == TAG_POINTER {
            unsafe { self.deref().as_closure() }
        } else {
            None
        }
    }

    pub fn as_escape(&self) -> Option<&'static Escape> {
        if self.ptr & TAG_MASK == TAG_POINTER {
            unsafe { self.deref().as_escape() }
        } else {
            None
        }
    }

    pub fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }

    pub fn eqv(&self, other: &Self) -> bool {
        if self.eq(other) {
            return true;
        }

        match (self.as_value(), other.as_value()) {
            (None, None) => {}
            (Some(a), Some(b)) => return a == b,
            _ => return false,
        }

        false
    }

    pub fn equal(&self, other: &Self) -> bool {
        if self.eq(other) {
            return true;
        }

        match (self.as_pair(), other.as_pair()) {
            (None, None) => {}
            (Some(a), Some(b)) => return a.0.equal(&b.0) && a.1.equal(&b.1),
            _ => return false,
        }

        match (self.as_value(), other.as_value()) {
            (None, None) => {}
            (Some(a), Some(b)) => return a == b,
            _ => return false,
        }

        false
    }

    pub fn numeq(&self, rhs: &Self) -> bool {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => a == b,
            _ => panic!("Type Error"),
        }
    }

    pub fn less(&self, rhs: &Self) -> bool {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => a < b,
            _ => panic!("Type Error"),
        }
    }

    pub fn less_eq(&self, rhs: &Self) -> bool {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => a <= b,
            _ => panic!("Type Error"),
        }
    }

    pub fn greater(&self, rhs: &Self) -> bool {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => a > b,
            _ => panic!("Type Error"),
        }
    }

    pub fn greater_eq(&self, rhs: &Self) -> bool {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => a >= b,
            _ => panic!("Type Error"),
        }
    }

    pub fn add(&self, rhs: &Self) -> Self {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => Self::int(a + b),
            _ => panic!("Type Error"),
        }
    }

    pub fn sub(&self, rhs: &Self) -> Self {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => Self::int(a - b),
            _ => panic!("Type Error"),
        }
    }

    pub fn mul(&self, rhs: &Self) -> Self {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => Self::int(dbg!(a) * dbg!(b)),
            _ => panic!("Type Error"),
        }
    }

    pub fn div(&self, rhs: &Self) -> Self {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => Self::int(dbg!(a) / dbg!(b)),
            _ => panic!("Type Error"),
        }
    }

    pub unsafe fn set_car_unchecked(&self, car: Scm) {
        let r: *const Scm = int_to_ref(self.ptr - TAG_PAIR);
        *(r as *mut _) = car;
    }

    pub unsafe fn set_cdr_unchecked(&self, cdr: Scm) {
        let r: *const Scm = int_to_ref(self.ptr - TAG_PAIR);
        *(r as *mut Scm).offset(1) = cdr;
    }

    pub fn car(&self) -> Option<Self> {
        self.as_pair().map(|(x, _)| *x)
    }

    pub fn cdr(&self) -> Option<Self> {
        self.as_pair().map(|(_, x)| *x)
    }

    /*pub unsafe fn car_unchecked(&self) -> Self {
        *int_to_ref(self.ptr - TAG_PAIR)
    }

    pub unsafe fn cdr_unchecked(&self) -> Self {
        *int_to_ref(self.ptr - TAG_PAIR + std::mem::size_of::<Scm>() as isize)
    }*/
}

impl OpaqueCast for Scm {
    unsafe fn from_op(op: OpaquePointer) -> Self {
        Scm {
            ptr: op.as_usize() as isize,
        }
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer::new(self.ptr as usize)
    }
}

impl From<&lexpr::Value> for Scm {
    fn from(exp: &lexpr::Value) -> Self {
        use lexpr::Value::*;
        match exp {
            Null => Scm::null(),
            Number(n) if n.is_i64() => Scm::int(n.as_i64().unwrap()),
            Cons(pair) => Scm::cons(pair.car().into(), pair.cdr().into()),
            Symbol(s) => Scm::symbol(s),
            String(s) => Scm::string(s),
            _ => unimplemented!(),
        }
    }
}

unsafe fn int_to_ref<T>(i: isize) -> &'static T {
    &*(i as *const T)
}

fn ref_to_int<T>(v: &T) -> isize {
    ptr_to_int(v)
}

fn ptr_to_int<T>(v: *const T) -> isize {
    v as isize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_alignment_is_large_enough_for_tags() {
        let value_alignment = std::mem::align_of::<ScmBoxedValue>();
        assert_eq!(
            value_alignment % (1 << N_TAG_BITS),
            0,
            "value alignment is {} but must be a multiple of {}",
            value_alignment,
            (1 << N_TAG_BITS)
        );
    }

    #[test]
    fn successive_values_are_aligned_correctly() {
        let values = vec![
            ScmBoxedValue::Symbol("foo"),
            ScmBoxedValue::String(Box::leak(Box::new("bar".to_string()))),
            ScmBoxedValue::Escape(Escape::allocate(42)),
        ];

        assert_eq!(ref_to_int(&values[0]) & TAG_MASK, TAG_POINTER);
        assert_eq!(ref_to_int(&values[1]) & TAG_MASK, TAG_POINTER);
        assert_eq!(ref_to_int(&values[2]) & TAG_MASK, TAG_POINTER);
    }

    #[test]
    fn zero_int_tagged_correctly() {
        let x = Scm::int(0);
        assert_eq!(x.ptr, TAG_INTEGER);
    }

    #[test]
    fn positive_int_tagged_correctly() {
        let x = Scm::int(1);
        assert_eq!(x.ptr & TAG_MASK, TAG_INTEGER);
    }

    #[test]
    fn negative_int_tagged_correctly() {
        let x = Scm::int(-1);
        assert_eq!(x.ptr & TAG_MASK, TAG_INTEGER);
    }

    #[test]
    fn signed_bitshift_behaves_as_expected() {
        assert_eq!(1_i64 << 3, 8);
        assert_eq!(8_i64 >> 3, 1);
        assert_eq!(-1_i64 << 3, -8);
        assert_eq!(-8_i64 >> 3, -1);

        assert_eq!(1_isize << N_TAG_BITS & TAG_MASK, 0);
        assert_eq!(-1_isize << N_TAG_BITS & TAG_MASK, 0);
    }

    #[test]
    fn integer_cast_behaves_as_expected() {
        assert_eq!(-1isize as usize, usize::max_value());
    }

    #[test]
    fn integers_are_correctly_represented() {
        fn check_int(i: i64) {
            let x = Scm::int(i);
            assert_eq!(x.as_int(), Some(i));
        }

        check_int(0);
        check_int(1);
        check_int(SCM_MAX_INT);
        check_int(-1);
        check_int(SCM_MIN_INT);
    }
}
