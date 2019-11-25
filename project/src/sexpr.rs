use crate::parsing::{parse, Sexpr as PS, Span, SpannedSexpr};
use crate::source::{Source, SourceLocation};
use crate::symbol::Symbol;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct TrackedSexpr {
    sexpr: Sexpr,
    src: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum Sexpr {
    Nil,
    Symbol(Symbol),
    Int(i64),

    List(RcSlice<TrackedSexpr>, Option<Box<TrackedSexpr>>),
}

impl std::fmt::Display for TrackedSexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.sexpr, f)
    }
}

impl std::fmt::Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Sexpr::Nil => write!(f, "'()"),
            Sexpr::Symbol(s) => write!(f, "{}", s),
            Sexpr::Int(i) => write!(f, "{}", i),
            Sexpr::List(items, dot) => {
                write!(f, "(")?;
                write!(f, "{}", items[0])?;
                for i in &items[1..] {
                    write!(f, " {}", i)?;
                }
                if let Some(d) = dot {
                    write!(f, " . {}", d)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl TrackedSexpr {
    pub fn from_source(source: Source) -> Self {
        let expr = match parse(&source.content) {
            Ok(x) => x,
            Err(e) => panic!("{:#?}", e),
        };
        Self::from_spanned(expr, source.clone())
    }

    pub fn from_spanned(se: SpannedSexpr, source: Source) -> Self {
        match se.expr {
            PS::Nil => TrackedSexpr::new(Sexpr::Nil, SourceLocation::from_spanned(se.span, source)),
            PS::Symbol(s) => TrackedSexpr::new(
                Sexpr::Symbol(s.into()),
                SourceLocation::from_spanned(se.span, source),
            ),
            PS::Integer(i) => {
                TrackedSexpr::new(Sexpr::Int(i), SourceLocation::from_spanned(se.span, source))
            }
            PS::List(l) if l.is_empty() => {
                TrackedSexpr::new(Sexpr::Nil, SourceLocation::from_spanned(se.span, source))
            }
            PS::List(l) => {
                let mut l = l.into_iter();
                let mut items = vec![];
                let mut dotted = None;
                while let Some(x) = l.next() {
                    if let PS::Dot = x.expr {
                        dotted = l
                            .next()
                            .map(|s| Box::new(Self::from_spanned(s, source.clone())));
                        break;
                    }
                    items.push(Self::from_spanned(x, source.clone()));
                }
                TrackedSexpr::new(
                    Sexpr::List(items.into(), dotted),
                    SourceLocation::from_spanned(se.span, source),
                )
            }
            _ => unimplemented!(),
        }
    }

    pub fn new(sexpr: Sexpr, src: SourceLocation) -> Self {
        TrackedSexpr { sexpr, src }
    }

    pub fn into_sexpr(self) -> Sexpr {
        self.sexpr
    }

    pub fn source(&self) -> &SourceLocation {
        &self.src
    }

    pub fn nil(src: SourceLocation) -> Self {
        TrackedSexpr {
            sexpr: Sexpr::Nil,
            src,
        }
    }

    pub fn first(&self) -> Option<&Self> {
        match &self.sexpr {
            Sexpr::List(l, _) if l.len() == 0 => panic!("invalid list"),
            Sexpr::List(l, _) => Some(&l[0]),
            _ => None,
        }
    }

    pub fn tail(&self) -> Option<Self> {
        match &self.sexpr {
            Sexpr::List(l, _) if l.len() == 0 => panic!("invalid list"),
            Sexpr::List(l, None) if l.len() == 1 => Some(Self::nil(self.src.last_char())),
            Sexpr::List(l, Some(dot)) if l.len() == 1 => Some((**dot).clone()),
            Sexpr::List(l, d) => Some(TrackedSexpr {
                sexpr: Sexpr::List(l.clone().slice_from(1), d.clone()),
                src: self.src.clone().start_at(&l[0].src),
            }),
            _ => None,
        }
    }

    pub fn at(&self, idx: usize) -> Option<&Self> {
        match &self.sexpr {
            Sexpr::List(l, _) if l.len() > idx => Some(&l[idx]),
            _ => None,
        }
    }

    pub fn is_atom(&self) -> bool {
        match &self.sexpr {
            Sexpr::List(_, _) => false,
            _ => true,
        }
    }

    pub fn is_symbol(&self) -> bool {
        self.as_symbol().is_some()
    }

    pub fn as_symbol(&self) -> Option<&Symbol> {
        match &self.sexpr {
            Sexpr::Symbol(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_pair(&self) -> Option<(Self, Self)> {
        let car = self.first().cloned();
        let cdr = self.tail();
        car.into_iter().zip(cdr).next()
    }

    pub fn as_list(&self) -> Option<(&RcSlice<Self>, Option<Self>)> {
        match &self.sexpr {
            Sexpr::List(l, dotted) => Some((l, dotted.as_ref().map(|s| (**s).clone()))),
            _ => None,
        }
    }

    pub fn as_proper_list(&self) -> Option<&RcSlice<Self>> {
        match &self.sexpr {
            Sexpr::List(l, None) => Some(l),
            _ => None,
        }
    }
}

impl From<lexpr::Value> for Sexpr {
    fn from(x: lexpr::Value) -> Self {
        use lexpr::Value::*;
        match x {
            Null => Sexpr::Nil,
            Number(ref n) if n.is_i64() => Sexpr::Int(n.as_i64().unwrap()),
            Symbol(s) => Sexpr::Symbol(s.into()),
            Cons(_) => {
                let mut list: Vec<TrackedSexpr> = vec![];

                let mut item = &x;
                while let Cons(c) = item {
                    list.push(c.car().clone().into());
                    item = c.cdr();
                }

                if item.is_null() {
                    Sexpr::List(list.into(), None)
                } else {
                    Sexpr::List(list.into(), Some(Box::new(item.clone().into())))
                }
            }
            _ => unimplemented!("{:?}", x),
        }
    }
}

impl From<lexpr::Value> for TrackedSexpr {
    fn from(x: lexpr::Value) -> Self {
        TrackedSexpr {
            sexpr: x.into(),
            src: SourceLocation::NoSource,
        }
    }
}

pub struct RcSlice<T> {
    base: Rc<[T]>,
    slice: *const [T],
}

impl<T: std::fmt::Debug> std::fmt::Debug for RcSlice<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", &**self)
    }
}

impl<T> Clone for RcSlice<T> {
    fn clone(&self) -> Self {
        RcSlice {
            base: self.base.clone(),
            slice: self.slice,
        }
    }
}

impl<T> From<Vec<T>> for RcSlice<T> {
    fn from(v: Vec<T>) -> Self {
        let src: Rc<_> = v.into();
        RcSlice {
            slice: &*src,
            base: src,
        }
    }
}

impl<T> std::ops::Deref for RcSlice<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { &*self.slice }
    }
}

impl<T> RcSlice<T> {
    pub fn slice(self, from: usize, to: usize) -> Self {
        RcSlice {
            base: self.base,
            slice: unsafe { &(*self.slice)[from..to] },
        }
    }

    pub fn slice_to(self, to: usize) -> Self {
        self.slice(0, to)
    }

    pub fn slice_from(self, from: usize) -> Self {
        let n = self.len();
        self.slice(from, n)
    }

    pub fn pop(&mut self) -> Option<&T> {
        let mut n = self.len();
        if n == 0 {
            return None;
        }
        n -= 1;

        let item = &self[n] as *const _;
        *self = self.clone().slice_to(n);

        unsafe { Some(&*item) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_it() {
        let src = Source::from("( x )");
        println!("{:?}", TrackedSexpr::from_source(src));
    }
}
