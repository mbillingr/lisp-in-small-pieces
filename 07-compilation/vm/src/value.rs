use crate::{ActivationFrame, Closure, CodePointer, OpaqueCast, OpaquePointer};
use lisp_core::lexpr;
use std::cell::UnsafeCell;
use std::mem::MaybeUninit;
use std::os::raw::c_void;

//pub const DYNENV_TAG: Value = Value::Symbol("*dynenv*");
pub const DYNENV_TAG: Scm = Scm { ptr: 123 };

const N_TAG_BITS: usize = 3;
const TAG_MASK: usize = 0b_111;
const TAG_POINTER: usize = 0b_000;
const TAG_INTEGER: usize = 0b_001;
const TAG_PAIR: usize = 0b_010;
//const TAG_FRAME: usize = 0b_100;
//const TAG_CLOSURE: usize = 0b_110;
const TAG_SPECIAL: usize = 0b_111;

const SPECIAL_UNINIT: usize = 0b_0000_0111;
const SPECIAL_NULL: usize = 0b_0001_0111;
const SPECIAL_FALSE: usize = 0b_0010_0111;
const SPECIAL_TRUE: usize = 0b_0011_0111;

const MASK_IMMEDIATE: usize = 0b001; // this works because all immediates have 1 in the lsb

#[derive(Debug, Copy, Clone)]
pub struct Scm {
    ptr: usize,
}

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Null,
    Uninitialized,
    Symbol(&'static str),
    String(&'static String),

    Frame(&'static ActivationFrame),
    Closure(&'static Closure),
    Pointer(&'static u8),
}

impl Scm {
    pub fn from_static_value(value: &'static Value) -> Self {
        Scm {
            ptr: value as *const _ as usize,
        }
    }

    pub fn from_value(value: Value) -> Self {
        Scm {
            ptr: Box::leak(Box::new(value)) as *const _ as usize,
        }
    }

    pub fn null() -> Self {
        Scm { ptr: SPECIAL_NULL }
    }

    pub fn uninitialized() -> Self {
        Scm {
            ptr: SPECIAL_UNINIT,
        }
    }

    pub fn bool(b: bool) -> Self {
        Scm { ptr: match b {
            true => SPECIAL_TRUE,
            false => SPECIAL_FALSE,
        }
    }
    }

    pub fn int(i: i64) -> Self {
        Scm {
            ptr: (i as usize) << N_TAG_BITS | TAG_INTEGER,
        }
    }

    pub fn cons(car: Scm, cdr: Scm) -> Self {
        //let r = Box::leak(Box::new((car, cdr)));
        let r = PAIR_ALLOCATOR.with(|pa| pa.alloc((car, cdr)));
        let addr = r as *const _ as usize;
        debug_assert!(addr & TAG_MASK == 0);
        Scm {
            ptr: addr + TAG_PAIR,
        }
    }

    pub fn symbol(s: &str) -> Self {
        let s = Box::leak(Box::new(s.to_owned()));
        Scm::from_value(Value::Symbol(s))
    }

    pub fn string(s: &str) -> Self {
        let s = Box::leak(Box::new(s.to_owned()));
        Scm::from_value(Value::String(s))
    }

    pub fn frame(size: usize) -> Self {
        let frm = ActivationFrame::allocate(size);
        Scm::from_value(Value::Frame(frm))
    }

    pub fn closure(code: CodePointer, env: &'static ActivationFrame) -> Self {
        let cls = Closure::allocate(code, env);
        Self::from_value(Value::Closure(cls))
    }

    pub fn pointer(ptr: &'static u8) -> Self {
        Self::from_value(Value::Pointer(ptr))
    }

    fn as_ptr(&self) -> *const Value {
        self.ptr as *const Value
    }

    unsafe fn deref(&self) -> &Value {
        unsafe { &*self.as_ptr() }
    }

    fn is_immediate(&self) -> bool {
        self.ptr & MASK_IMMEDIATE != 0
    }

    pub fn is_false(&self) -> bool {
        self.ptr == SPECIAL_FALSE
    }

    pub fn is_uninitialized(&self) -> bool {
        unsafe { (*self.as_ptr()).is_uninitialized() }
    }

    pub fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }

    pub fn is_closure(&self) -> bool {
        self.as_closure().is_some()
    }

    fn as_int(&self) -> Option<i64> {
        if self.ptr & TAG_MASK == TAG_INTEGER {
            Some((self.ptr >> N_TAG_BITS) as i64)
        } else {
            None
        }
    }

    fn as_pair(&self) -> Option<&(Scm, Scm)> {
        if self.ptr & TAG_MASK == TAG_PAIR {
            unsafe { Some(int_to_ref(self.ptr - TAG_PAIR)) }
        } else {
            None
        }
    }

    pub fn as_frame(&self) -> Option<&'static ActivationFrame> {
        unsafe { self.deref().as_frame() }
    }

    pub fn as_closure(&self) -> Option<&'static Closure> {
        unsafe { self.deref().as_closure() }
    }

    pub fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }

    pub fn eqv(&self, other: &Self) -> bool {
        self.eq(other) || unsafe { self.deref() == other.deref() }
    }

    pub fn numeq(&self, rhs: &Self) -> Self {
        Self::bool(self.eq(rhs))
    }

    pub fn less(&self, rhs: &Self) -> Self {
        match (self.as_int(), rhs.as_int()) {
            (Some(a), Some(b)) => Self::bool(a < b),
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
            (Some(a), Some(b)) => Self::int(a * b),
            _ => panic!("Type Error"),
        }
    }
}

impl OpaqueCast for Scm {
    unsafe fn from_op(op: OpaquePointer) -> Self {
        Scm { ptr: op.0 }
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer(self.ptr)
    }
}

impl Value {
    pub fn is_uninitialized(&self) -> bool {
        match self {
            Value::Uninitialized => true,
            _ => false,
        }
    }

    pub fn as_pointer(&self) -> Option<*const u8> {
        match self {
            Value::Pointer(p) => Some(*p),
            _ => None,
        }
    }

    pub fn as_frame(&self) -> Option<&'static ActivationFrame> {
        match self {
            Value::Frame(frame) => Some(frame),
            _ => None,
        }
    }

    pub fn as_closure(&self) -> Option<&'static Closure> {
        match self {
            Value::Closure(cls) => Some(cls),
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Null, Null) => true,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            _ => false,
        }
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

unsafe fn int_to_ref<T>(i: usize) -> &'static T {
    &*(i as *const T)
}

fn ref_to_addr<T>(r: &T) -> usize {
    r as *const T as usize
}

// ======= BATCH ALLOCATION ==========

thread_local! {
    static PAIR_ALLOCATOR: BatchAllocator<(Scm, Scm)> = BatchAllocator::new();
}

#[link(name = "gc", kind = "static")]
extern "C" {
    fn GC_malloc_many(n_bytes: usize) -> *mut c_void;
}

struct BatchAllocator<T: Copy> {
    free_list: UnsafeCell<*mut T>,
}

impl<T: Copy> BatchAllocator<T> {
    fn new() -> Self {
        BatchAllocator {
            free_list: UnsafeCell::new(0 as *mut _),
        }
    }

    fn alloc(&self, value: T) -> &'static mut T {
        unsafe {
            let mut x = self.alloc_uninit();
            *x = value;
            x
        }
    }

    fn alloc_with<F: FnOnce(&mut T)>(&self, f: F) -> &'static mut T {
        unsafe {
            let x = self.alloc_uninit();
            f(&mut *x);
            x
        }
    }

    unsafe fn alloc_uninit(&self) -> &'static mut T {
        self.ensure_list();
        &mut *self.next_item()
    }

    unsafe fn ensure_list(&self) {
        if *self.free_list.get() == 0 as *mut _ {
            *self.free_list.get() = GC_malloc_many(std::mem::size_of::<T>()) as *mut T;
            debug_assert_ne!(*self.free_list.get(), 0 as *mut _);
        }
    }

    unsafe fn next_item(&self) -> *mut T {
        let item = *self.free_list.get();
        *self.free_list.get() = *(*self.free_list.get() as *mut *mut T);
        item
    }
}

impl std::fmt::Display for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match (self.ptr, self.ptr & TAG_MASK) {
            (SPECIAL_UNINIT, _) => write!(f, "*uninit*"),
            (SPECIAL_NULL, _) => write!(f, "'()"),
            (SPECIAL_FALSE, _) => write!(f, "#f"),
            (SPECIAL_TRUE, _) => write!(f, "'#t"),
            (_, TAG_INTEGER) => write!(f, "{}", self.as_int().unwrap()),
            (_, TAG_PAIR) => {
                let (car, cdr) = self.as_pair().unwrap();
                write!(f, "({} . {})", car, cdr)
            },
            (_, TAG_POINTER) => {
                write!(f, "{:p}", self.ptr as *const u8)
            },
            (_, _) => write!(f, "*invalid*"),
        }
    }
}