use crate::ast::RuntimePrimitive;
use crate::bytecode::{Closure, CodeObject};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::symbol::Symbol;
use std::cell::Cell;

#[derive(Debug, Copy, Clone)]
pub enum Scm {
    Undefined,
    Uninitialized,
    Nil,
    True,
    False,
    Int(i64),
    Float(f64),
    Symbol(Symbol),
    String(&'static str),
    Vector(&'static [Cell<Scm>]),

    Pair(&'static (Cell<Scm>, Cell<Scm>)),

    Closure(&'static Closure),
    Primitive(RuntimePrimitive),

    /*Procedure(RuntimeProcedure),
    Primitive(RuntimePrimitive),*/
    Cell(&'static Cell<Scm>),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    TypeError,
}

impl Scm {
    pub fn uninitialized() -> Self {
        Scm::Uninitialized
    }

    pub fn nil() -> Self {
        Self::Nil
    }

    pub fn bool(b: bool) -> Self {
        match b {
            true => Scm::True,
            false => Scm::False,
        }
    }

    pub fn boxed(x: Scm) -> Scm {
        Scm::Cell(Box::leak(Box::new(Cell::new(x))))
    }

    pub fn cons(car: Scm, cdr: Scm) -> Scm {
        Scm::Pair(Box::leak(Box::new((Cell::new(car), Cell::new(cdr)))))
    }

    pub fn closure(func: &'static CodeObject, free_vars: impl Into<Box<[Scm]>>) -> Self {
        Scm::Closure(Box::leak(Box::new(Closure::new(func, free_vars.into()))))
    }

    pub fn string(s: impl Into<Box<str>>) -> Self {
        Scm::String(Box::leak(s.into()))
    }

    pub fn list(items: impl IntoIterator<Item = Scm>) -> Self {
        let mut head = Scm::Nil;
        let mut last_pair = Scm::Nil;
        for x in items {
            if last_pair.is_nil() {
                head = Scm::cons(x, Scm::Nil);
                last_pair = head;
            } else {
                let new_pair = Scm::cons(x, Scm::Nil);
                last_pair.set_cdr(new_pair);
                last_pair = new_pair;
            }
        }
        head
    }

    pub fn vector(items: impl IntoIterator<Item = Scm>) -> Self {
        let v: Vec<Cell<Scm>> = items.into_iter().map(Cell::new).collect();
        let static_data = Box::leak(v.into_boxed_slice());
        Scm::Vector(static_data)
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Scm::Nil => true,
            _ => false,
        }
    }

    pub fn is_false(&self) -> bool {
        match self {
            Scm::False => true,
            _ => false,
        }
    }

    pub fn car(&self) -> Option<Scm> {
        match self {
            Scm::Pair(p) => Some(p.0.get()),
            _ => None,
        }
    }

    pub fn cdr(&self) -> Option<Scm> {
        match self {
            Scm::Pair(p) => Some(p.1.get()),
            _ => None,
        }
    }

    pub fn set_car(&self, x: Scm) -> Option<()> {
        match self {
            Scm::Pair(p) => {
                p.0.set(x);
                Some(())
            }
            _ => None,
        }
    }

    pub fn set_cdr(&self, x: Scm) -> Option<()> {
        match self {
            Scm::Pair(p) => {
                p.1.set(x);
                Some(())
            }
            _ => None,
        }
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        use Scm::*;
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Symbol(a), Symbol(b)) => a.ptr_eq(b),
            (String(a), String(b)) => *a as *const str == *b as *const str,
            (Vector(a), Vector(b)) => *a as *const _ == *b as *const _,
            (Pair(a), Pair(b)) => *a as *const _ == *b as *const _,
            (Cell(a), Cell(b)) => *a as *const _ == *b as *const _,
            _ => false,
        }
    }

    pub fn equals(&self, other: &Self) -> bool {
        use Scm::*;
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Vector(a), Vector(b)) => a.iter().zip(*b).all(|(a, b)| a.get().equals(&b.get())),
            (Pair(a), Pair(b)) => a.0.get().equals(&b.0.get()) && a.1.get().equals(&b.1.get()),
            (Cell(a), Cell(b)) => a.get().equals(&b.get()),
            _ => false,
        }
    }

    pub fn num_less(&self, other: &Self) -> Result<bool> {
        use Scm::*;
        match (*self, *other) {
            (Int(a), Int(b)) => Ok(a < b),
            (Int(a), Float(b)) => Ok((a as f64) < b),
            (Float(a), Int(b)) => Ok(a < (b as f64)),
            (Float(a), Float(b)) => Ok(a < b),
            _ => Err(Error::TypeError),
        }
    }

    pub fn set(&self, value: Scm) -> Result<()> {
        match self {
            Scm::Cell(x) => Ok(x.set(value)),
            _ => Err(Error::TypeError),
        }
    }

    pub fn get(&self) -> Result<Scm> {
        match self {
            Scm::Cell(x) => Ok(x.get()),
            _ => Err(Error::TypeError),
        }
    }
}

impl std::fmt::Display for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Scm::Undefined => write!(f, "*undefined*"),
            Scm::Uninitialized => write!(f, "*uninitialized*"),
            Scm::Nil => write!(f, "'()"),
            Scm::True => write!(f, "#t"),
            Scm::False => write!(f, "#f"),
            Scm::Int(i) => write!(f, "{}", i),
            Scm::Float(x) => write!(f, "{}", x),
            Scm::Symbol(s) => write!(f, "{}", s),
            Scm::String(s) => write!(f, "{}", s),
            Scm::Vector(v) => {
                write!(f, "#(")?;
                let mut items = v.iter();
                if let Some(x) = items.next() {
                    write!(f, "{}", x.get())?;

                    for x in items {
                        write!(f, " {}", x.get())?;
                    }
                }
                write!(f, ")")
            }
            Scm::Pair(p) => {
                //write!(f, "({} . {})", p.0.get(), p.1.get())
                write!(f, "({}", p.0.get())?;
                let mut cdr = p.1.get();
                loop {
                    match cdr {
                        Scm::Nil => break,
                        Scm::Pair(q) => {
                            write!(f, " {}", q.0.get())?;
                            cdr = q.1.get();
                        }
                        x => {
                            write!(f, " . {}", x)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Scm::Closure(cls) => write!(f, "<closure {:p}>", *cls),
            Scm::Primitive(prim) => write!(f, "<primitive {:p}>", &prim.func),
            Scm::Cell(c) => write!(f, "{}", c.get()),
        }
    }
}

impl From<&Sexpr> for Scm {
    fn from(e: &Sexpr) -> Self {
        match e {
            Sexpr::Nil => Scm::Nil,
            Sexpr::True => Scm::True,
            Sexpr::False => Scm::False,
            Sexpr::Int(i) => Scm::Int(*i),
            Sexpr::Float(f) => Scm::Float(*f),
            Sexpr::Symbol(s) => Scm::Symbol(*s),
            Sexpr::String(s) => Scm::string(&**s),
            Sexpr::List(l, dot) => {
                let mut x = dot.as_ref().map(|d| (&d.sexpr).into()).unwrap_or(Scm::Nil);
                for item in l.iter().rev() {
                    x = Scm::cons(item.into(), x);
                }
                x
            }
            Sexpr::Vector(v) => {
                let items: Vec<Cell<Scm>> = v.iter().map(|i| Cell::new(i.into())).collect();
                let items = items.into_boxed_slice();
                Scm::Vector(Box::leak(items))
            }
            _ => unimplemented!(),
        }
    }
}

impl From<&TrackedSexpr> for Scm {
    fn from(e: &TrackedSexpr) -> Self {
        (&e.sexpr).into()
    }
}

impl std::ops::Mul for Scm {
    type Output = Result<Scm>;
    fn mul(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Int(a * b)),
            (Int(a), Float(b)) => Ok(Float(a as f64 * b)),
            (Float(a), Int(b)) => Ok(Float(a * b as f64)),
            (Float(a), Float(b)) => Ok(Float(a * b)),
            _ => Err(Error::TypeError),
        }
    }
}

impl std::ops::Div for Scm {
    type Output = Result<Scm>;
    fn div(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Float(a as f64 / b as f64)),
            (Int(a), Float(b)) => Ok(Float(a as f64 / b)),
            (Float(a), Int(b)) => Ok(Float(a / b as f64)),
            (Float(a), Float(b)) => Ok(Float(a / b)),
            _ => Err(Error::TypeError),
        }
    }
}

impl std::ops::Add for Scm {
    type Output = Result<Scm>;
    fn add(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Int(a + b)),
            (Int(a), Float(b)) => Ok(Float(a as f64 + b)),
            (Float(a), Int(b)) => Ok(Float(a + b as f64)),
            (Float(a), Float(b)) => Ok(Float(a + b)),
            _ => Err(Error::TypeError),
        }
    }
}

impl std::ops::Sub for Scm {
    type Output = Result<Scm>;
    fn sub(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Int(a - b)),
            (Int(a), Float(b)) => Ok(Float(a as f64 - b)),
            (Float(a), Int(b)) => Ok(Float(a - b as f64)),
            (Float(a), Float(b)) => Ok(Float(a - b)),
            _ => Err(Error::TypeError),
        }
    }
}
