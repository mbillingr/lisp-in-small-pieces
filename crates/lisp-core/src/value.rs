use crate::{Context, ErrorKind, LispData, LispResult, Managable};

#[derive(Copy, Clone)]
pub enum LispValue {
    Undefined,
    Nil,
    True,
    False,
    Integer(i64),
    Symbol(&'static &'static str), // this indirection is meant to make the type smaller &str = 2 words but &&str = 1 word. todo: test which is better

    Record(*mut Self, u32),
    Pair(*mut Self),

    ByteArray(*mut u8, u32),
    String(*mut u8, u32),

    Function(fn(LispValue, &mut Context<LispValue>) -> LispValue),

    Relocated(*const Self),
}

impl LispValue {
    pub fn into_str(self) -> Option<Self> {
        match self {
            LispValue::ByteArray(data, len) => Some(LispValue::String(data, len)),
            _ => None,
        }
    }
}

impl Default for LispValue {
    fn default() -> Self {
        LispValue::Undefined
    }
}

impl PartialEq for LispValue {
    fn eq(&self, rhs: &Self) -> bool {
        use LispValue::*;
        match (self, rhs) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Integer(a), Integer(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Record(a, n), Record(b, m)) => a == b && n == m,
            (Pair(a), Pair(b)) => a == b,
            _ => false,
        }
    }
}

impl LispData for LispValue {
    type Integer = i64;
    type Symbol = &'static &'static str;
    type ByteArray = (*mut u8, u32);

    fn is_nil(&self) -> bool {
        match self {
            LispValue::Nil => true,
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            LispValue::True | LispValue::False => true,
            _ => false,
        }
    }

    fn is_true(&self) -> bool {
        match self {
            LispValue::True => true,
            _ => false,
        }
    }

    fn is_number(&self) -> bool {
        match self {
            LispValue::Integer(_) => true,
            _ => false,
        }
    }

    fn is_symbol(&self) -> bool {
        match self {
            LispValue::Symbol(_) => true,
            _ => false,
        }
    }

    fn is_pair(&self) -> bool {
        match self {
            LispValue::Pair(_) => true,
            _ => false,
        }
    }

    fn undefined() -> Self {
        LispValue::Undefined
    }

    fn nil() -> Self {
        LispValue::Nil
    }

    fn bool(b: bool) -> Self {
        match b {
            true => LispValue::True,
            false => LispValue::False,
        }
    }

    fn int(i: i64) -> Self {
        LispValue::Integer(i)
    }

    fn char(_: char) -> Self {
        unimplemented!()
    }

    fn symbol(s: &str) -> Self {
        let boxed: Box<str> = s.into();
        let bboxed = Box::new(Box::leak(boxed) as &_);
        LispValue::Symbol(Box::leak(bboxed))
    }

    fn keyword(_: &str) -> Self {
        unimplemented!()
    }

    fn set_car(&self, item: Self) -> LispResult<()> {
        match self {
            LispValue::Pair(p) => unsafe { **p = item },
            _ => return ErrorKind::NoPair.into(),
        }
        Ok(())
    }

    fn set_cdr(&self, item: Self) -> LispResult<()> {
        match self {
            LispValue::Pair(p) => unsafe { *p.offset(1) = item },
            _ => return ErrorKind::NoPair.into(),
        }
        Ok(())
    }

    fn set_array_item(&self, index: usize, item: Self) -> LispResult<()> {
        match self {
            LispValue::Record(p, n) => {
                if index > *n as usize {
                    return ErrorKind::OutOfBounds(index, *n as usize).into();
                }
                unsafe { *p.offset(index as isize) = item }
            }
            LispValue::Pair(p) => {
                if index > 2 {
                    return ErrorKind::OutOfBounds(index, 2).into();
                }
                unsafe { *p.offset(index as isize) = item }
            }
            _ => return ErrorKind::NoRecord.into(),
        }
        Ok(())
    }

    fn car(&self) -> LispResult<Self> {
        match self {
            LispValue::Pair(p) => unsafe { Ok(*p.offset(0)) },
            _ => ErrorKind::NoPair.into(),
        }
    }

    fn cdr(&self) -> LispResult<Self> {
        match self {
            LispValue::Pair(p) => unsafe { Ok(*p.offset(1)) },
            _ => ErrorKind::NoPair.into(),
        }
    }

    fn get_array_item(&self, index: usize) -> LispResult<Self> {
        match self {
            LispValue::Record(p, n) => {
                if index > *n as usize {
                    ErrorKind::OutOfBounds(index, *n as usize).into()
                } else {
                    unsafe { Ok(*p.offset(index as isize)) }
                }
            }
            LispValue::Pair(p) => {
                if index > 2 {
                    ErrorKind::OutOfBounds(index, 2).into()
                } else {
                    unsafe { Ok(*p.offset(index as isize)) }
                }
            }
            _ => ErrorKind::NoRecord.into(),
        }
    }

    fn string_from_array(self) -> Option<Self> {
        match self {
            LispValue::ByteArray(ptr, n) => Some(LispValue::String(ptr, n)),
            _ => None,
        }
    }

    fn equals_str(&self, s: &str) -> bool {
        match self {
            LispValue::Symbol(y) => **y == s,
            _ => false,
        }
    }
}

impl Managable for LispValue {
    fn record(ptr: *mut Self, len: usize) -> Self {
        //todo: assert len does not overflow
        match len {
            2 => LispValue::Pair(ptr),
            _ => LispValue::Record(ptr, len as u32),
        }
    }

    fn array(ptr: *mut u8, len: usize) -> Self {
        //todo: assert len does not overflow
        LispValue::ByteArray(ptr, len as u32)
    }

    fn relocated(ptr: *const Self) -> Self {
        LispValue::Relocated(ptr)
    }

    unsafe fn as_record(&self) -> Option<&mut [Self]> {
        match *self {
            LispValue::Record(ptr, len) => Some(std::slice::from_raw_parts_mut(ptr, len as usize)),
            LispValue::Pair(ptr) => Some(std::slice::from_raw_parts_mut(ptr, 2)),
            _ => None,
        }
    }

    unsafe fn as_array(&self) -> Option<&mut [u8]> {
        match *self {
            LispValue::ByteArray(ptr, len) => {
                Some(std::slice::from_raw_parts_mut(ptr, len as usize))
            }
            _ => None,
        }
    }

    fn as_relocated(&self) -> Option<*const Self> {
        match *self {
            LispValue::Relocated(ptr) => Some(ptr),
            _ => None,
        }
    }
}

impl From<i64> for LispValue {
    fn from(i: i64) -> Self {
        LispValue::Integer(i)
    }
}

impl From<bool> for LispValue {
    fn from(b: bool) -> Self {
        match b {
            true => LispValue::True,
            false => LispValue::False,
        }
    }
}

impl std::fmt::Display for LispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LispValue::Undefined => write!(f, "#UNDEFINED"),
            LispValue::Nil => write!(f, "#NIL"),
            LispValue::True => write!(f, "#t"),
            LispValue::False => write!(f, "#f"),
            LispValue::Integer(i) => write!(f, "{}", i),
            LispValue::Symbol(s) => write!(f, "{}", s),
            LispValue::Pair(ptr) => {
                write!(f, "({} . {})", unsafe { **ptr }, unsafe { *ptr.offset(1) })
            }
            LispValue::Record(_, n) => write!(f, "{}-record", n),
            LispValue::ByteArray(ptr, n) => write!(f, "{:?}", unsafe {
                std::slice::from_raw_parts(*ptr, *n as usize)
            }),
            LispValue::String(ptr, n) => write!(f, "{:?}", unsafe {
                std::str::from_utf8(std::slice::from_raw_parts(*ptr, *n as usize)).unwrap()
            }),
            LispValue::Function(fptr) => write!(f, "fn<{:p}>", fptr),
            LispValue::Relocated(_) => write!(f, "\u{1f494}"),
        }
    }
}

impl std::fmt::Debug for LispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LispValue::Undefined => write!(f, "#UNDEFINED"),
            LispValue::Nil => write!(f, "#NIL"),
            LispValue::True => write!(f, "#t"),
            LispValue::False => write!(f, "#f"),
            LispValue::Integer(i) => write!(f, "{}", i),
            LispValue::Symbol(s) => write!(f, "{}", s),
            LispValue::Pair(ptr) => {
                write!(f, "({} . {})", unsafe { **ptr }, unsafe { *ptr.offset(1) })
            }
            LispValue::Record(_, n) => write!(f, "{}-record", n),
            LispValue::ByteArray(ptr, n) => write!(f, "{:?}", unsafe {
                std::slice::from_raw_parts(*ptr, *n as usize)
            }),
            LispValue::String(ptr, n) => write!(f, "{:?}", unsafe {
                std::str::from_utf8(std::slice::from_raw_parts(*ptr, *n as usize)).unwrap()
            }),
            LispValue::Function(fptr) => write!(f, "fn<{:p}>", fptr),
            LispValue::Relocated(_) => write!(f, "\u{1f494}"),
        }
    }
}
