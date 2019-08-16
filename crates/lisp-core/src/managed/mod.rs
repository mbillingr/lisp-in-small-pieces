mod context;
mod errors;
mod lisp_ops;
mod storage;
mod value;

pub use context::Context;
pub use errors::{ErrorKind, LispError, LispResult};
pub use lisp_ops::LispOps;
pub use value::LispValue;

pub trait LispData: Sized {
    type Integer;
    type Symbol;
    type ByteArray;

    fn is_nil(&self) -> bool;
    fn is_bool(&self) -> bool;
    fn is_true(&self) -> bool;
    fn is_number(&self) -> bool;
    fn is_symbol(&self) -> bool;
    fn is_pair(&self) -> bool;

    fn undefined() -> Self;
    fn nil() -> Self;
    fn bool(b: bool) -> Self;
    fn int(i: Self::Integer) -> Self;
    fn char(ch: char) -> Self;
    fn symbol(s: &str) -> Self;
    fn keyword(s: &str) -> Self;

    fn set_car(&self, item: Self) -> LispResult<()>;
    fn set_cdr(&self, item: Self) -> LispResult<()>;
    fn set_array_item(&self, index: usize, item: Self) -> LispResult<()>;

    fn car(&self) -> LispResult<Self>;
    fn cdr(&self) -> LispResult<Self>;
    fn get_array_item(&self, index: usize) -> LispResult<Self>;

    fn string_from_array(self) -> Option<Self>;

    fn equals_str(&self, s: &str) -> bool;
}

pub trait Managable: Sized + Copy + Default {
    fn record(ptr: *mut Self, len: usize) -> Self;
    fn array(ptr: *mut u8, len: usize) -> Self;
    fn relocated(ptr: *const Self) -> Self;

    unsafe fn as_array(&self) -> Option<&mut [u8]>;
    unsafe fn as_record(&self) -> Option<&mut [Self]>;
    fn as_relocated(&self) -> Option<*const Self>;

    fn is_record(&self) -> bool {
        unsafe { self.as_record().is_some() }
    }
    fn is_relocated(&self) -> bool {
        self.as_relocated().is_some()
    }
    fn is_array(&self) -> bool {
        unsafe { self.as_array().is_some() }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
