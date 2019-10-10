use crate::{ActivationFrame, Closure, CodePointer, OpaqueCast, OpaquePointer, Primitive};
use lisp_core::lexpr;

//pub const DYNENV_TAG: Value = Value::Symbol("*dynenv*");
pub const DYNENV_TAG: Scm = Scm { ptr: 123 };

const N_TAG_BITS: usize = 2;
const TAG_MASK: usize = 0b_11;
const TAG_POINTER: usize = 0b_00;
const TAG_INTEGER: usize = 0b_01;
const TAG_PAIR: usize = 0b_10;
//const TAG_SPECIAL: usize = 0b_11;

const SPECIAL_UNINIT: usize = 0b_0000_0111;
const SPECIAL_NULL: usize = 0b_0001_0111;
const SPECIAL_FALSE: usize = 0b_0010_0111;
const SPECIAL_TRUE: usize = 0b_0011_0111;

//const MASK_IMMEDIATE: usize = 0b01; // this works because all immediates have 1 in the lsb

thread_local! {
    static PAIR_ALLOCATOR: allocator::Allocator<(Scm, Scm)> = allocator::Allocator::new();
    static VALUE_ALLOCATOR: allocator::Allocator<Value> = allocator::Allocator::new();
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Scm {
    ptr: usize,
}

#[derive(Copy, Clone)]
pub enum Value {
    Symbol(&'static str),
    String(&'static String),

    Primitive(&'static Primitive),
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
        //let p = Box::leak(Box::new(value));
        let p = VALUE_ALLOCATOR.with(|pa| pa.alloc(value));
        Scm {
            ptr: p as *const _ as usize,
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
        Scm {
            ptr: match b {
                true => SPECIAL_TRUE,
                false => SPECIAL_FALSE,
            },
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

    pub unsafe fn set_car(&self, car: Scm) {
        let r: *const Scm = int_to_ref(self.ptr - TAG_PAIR);
        *(r as *mut _) = car;
    }

    pub unsafe fn set_cdr(&self, cdr: Scm) {
        let r: *const Scm = int_to_ref(self.ptr - TAG_PAIR);
        *(r as *mut Scm).offset(1) = cdr;
    }

    pub fn symbol(s: &str) -> Self {
        let s = Box::leak(Box::new(s.to_owned()));
        Scm::from_value(Value::Symbol(s))
    }

    pub fn string(s: &str) -> Self {
        let s = Box::leak(Box::new(s.to_owned()));
        Scm::from_value(Value::String(s))
    }

    pub fn primitive(p: Primitive) -> Self {
        let p = Box::leak(Box::new(p));
        Scm::from_value(Value::Primitive(p))
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
        &*self.as_ptr()
    }

    /*fn is_immediate(&self) -> bool {
        self.ptr & MASK_IMMEDIATE != 0
    }*/

    pub fn is_false(&self) -> bool {
        self.ptr == SPECIAL_FALSE
    }

    pub fn is_true(&self) -> bool {
        self.ptr == SPECIAL_TRUE
    }

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

    pub fn as_value(&self) -> Option<&Value> {
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
    pub fn as_pointer(&self) -> Option<*const u8> {
        match self {
            Value::Pointer(p) => Some(*p),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<&'static str> {
        match self {
            Value::Symbol(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_primitive(&self) -> Option<&'static Primitive> {
        match self {
            Value::Primitive(p) => Some(p),
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
            }
            (_, TAG_POINTER) => write!(f, "{}", unsafe { self.deref() }),
            (_, _) => write!(f, "*invalid*"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Symbol(s) => write!(f, "{}", s),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Primitive(p) => write!(f, "<primitive {}>", p.name),
            Value::Frame(af) => write!(f, "{:?}", af),
            Value::Closure(c) => write!(f, "{:?}", c),
            Value::Pointer(p) => write!(f, "{:p}", p),
        }
    }
}

#[cfg(not(feature = "batch-alloc"))]
mod allocator {
    use std::marker::PhantomData;

    pub type Allocator<T> = BoxAllocator<T>;

    pub struct BoxAllocator<T: Copy>(PhantomData<T>);

    impl<T: Copy> BoxAllocator<T> {
        pub fn new() -> Self {
            BoxAllocator(PhantomData)
        }

        pub fn alloc(&self, value: T) -> &'static mut T {
            Box::leak(Box::new(value))
        }
    }
}

#[cfg(feature = "batch-alloc")]
mod allocator {
    use std::cell::UnsafeCell;
    use std::os::raw::c_void;

    #[link(name = "gc", kind = "static")]
    extern "C" {
        fn GC_malloc_many(n_bytes: usize) -> *mut c_void;
    }

    pub type Allocator<T> = BatchAllocator<T>;

    pub struct BatchAllocator<T: Copy> {
        free_list: UnsafeCell<*mut T>,
    }

    impl<T: Copy> BatchAllocator<T> {
        pub fn new() -> Self {
            assert!(std::mem::size_of::<T>() >= std::mem::size_of::<usize>());
            BatchAllocator {
                free_list: UnsafeCell::new(0 as *mut _),
            }
        }

        pub fn alloc(&self, value: T) -> &'static mut T {
            unsafe {
                let x = self.alloc_uninit();
                std::ptr::write(x, value);
                &mut *x
            }
        }

        unsafe fn alloc_uninit(&self) -> *mut T {
            self.ensure_list();
            self.next_item()
        }

        unsafe fn ensure_list(&self) {
            if *self.free_list.get() == std::mem::transmute(0_usize) {
                let memory = GC_malloc_many(std::mem::size_of::<T>());
                *self.free_list.get() = std::mem::transmute(memory);
                assert_ne!(*self.free_list.get(), std::mem::transmute(0_usize));
            }
        }

        unsafe fn next_item(&self) -> *mut T {
            // C macro: #define GC_NEXT(p) (*(void * *)(p))
            let head = self.free_list.get();
            let item = *head;
            let next_item = *(item as *mut *mut T);
            *head = next_item;
            item
        }
    }
}
