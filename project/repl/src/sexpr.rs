use crate::error::{Error, Result, TypeError};
use crate::scm::Scm;
use crate::source::{Source, SourceLocation};
use crate::syntactic_closure::SyntacticClosure;
use std::fmt::Debug;
use std::rc::Rc;
use sunny_common::Symbol;
use sunny_parser::{parse, Sexpr as PS, SpannedSexpr};

#[derive(Debug, Clone)]
pub struct TrackedSexpr {
    pub sexpr: Sexpr,
    pub src: SourceLocation,
}

// TODO: Should we intern symbols and string?

#[derive(Debug, Clone, PartialEq)]
pub enum Sexpr {
    Undefined,
    Uninitialized,
    Nil,
    True,
    False,
    Char(char),
    Symbol(Symbol),
    String(Rc<str>),
    Int(i64),
    Float(f64),

    Pair(Box<(TrackedSexpr, TrackedSexpr)>),
    Vector(Vec<TrackedSexpr>),

    SyntacticClosure(Rc<SyntacticClosure>),
}

impl std::fmt::Display for TrackedSexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.sexpr, f)
    }
}

impl std::fmt::Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Sexpr::Undefined => write!(f, "*undefined*"),
            Sexpr::Uninitialized => write!(f, "*uninitialized*"),
            Sexpr::Nil => write!(f, "'()"),
            Sexpr::True => write!(f, "#t"),
            Sexpr::False => write!(f, "#f"),
            Sexpr::Char(ch) => write!(f, "#\\{}", ch),
            Sexpr::Symbol(s) => write!(f, "{}", s),
            Sexpr::String(s) => write!(f, "\"{}\"", s),
            Sexpr::Int(i) => write!(f, "{}", i),
            Sexpr::Float(i) => write!(f, "{}", i),
            Sexpr::Pair(p) => {
                write!(f, "(")?;
                write!(f, "{}", p.0)?;
                let mut p = &p.1;
                while let Some((car, cdr)) = p.as_pair() {
                    write!(f, " {}", car)?;
                    p = cdr;
                }
                if !p.is_null() {
                    write!(f, " . {}", p)?;
                }
                write!(f, ")")
            }
            Sexpr::Vector(items) => {
                write!(f, "#(")?;
                write!(f, "{}", items[0])?;
                for i in &items[1..] {
                    write!(f, " {}", i)?;
                }
                write!(f, ")")
            }
            Sexpr::SyntacticClosure(sc) => write!(f, "<{}>", sc.sexpr()),
        }
    }
}

impl TrackedSexpr {
    pub fn from_source(source: &Source) -> Result<Vec<Self>> {
        parse(&source.content)
            .map(|sexprs| {
                sexprs
                    .into_iter()
                    .map(|sexpr| Self::from_spanned(sexpr, source.clone()))
                    .collect()
            })
            .map_err(|e| Error::from_parse_error_and_source(e, source.clone()))
    }

    pub fn from_spanned(se: SpannedSexpr, source: Source) -> Self {
        match se.expr {
            PS::Nil => TrackedSexpr::new(Sexpr::Nil, SourceLocation::from_spanned(se.span, source)),
            PS::True => {
                TrackedSexpr::new(Sexpr::True, SourceLocation::from_spanned(se.span, source))
            }
            PS::False => {
                TrackedSexpr::new(Sexpr::False, SourceLocation::from_spanned(se.span, source))
            }
            PS::Char(ch) => TrackedSexpr::new(
                Sexpr::Char(ch),
                SourceLocation::from_spanned(se.span, source),
            ),
            PS::Symbol(s) => TrackedSexpr::new(
                Sexpr::Symbol(s.into()),
                SourceLocation::from_spanned(se.span, source),
            ),
            PS::String(s) => TrackedSexpr::escaped_string(s)
                .with_src(SourceLocation::from_spanned(se.span, source)),
            PS::Integer(i) => {
                TrackedSexpr::new(Sexpr::Int(i), SourceLocation::from_spanned(se.span, source))
            }
            PS::Float(f) => TrackedSexpr::new(
                Sexpr::Float(f),
                SourceLocation::from_spanned(se.span, source),
            ),
            PS::List(l) if l.is_empty() => {
                TrackedSexpr::new(Sexpr::Nil, SourceLocation::from_spanned(se.span, source))
            }
            PS::List(l) => {
                let mut out_list = TrackedSexpr::nil(
                    SourceLocation::from_spanned(se.span, source.clone()).last_char(),
                );
                for x in l.into_iter().rev() {
                    if let PS::Dot = x.expr {
                        out_list = out_list.decons().unwrap().0;
                    } else {
                        let src = out_list
                            .src
                            .start_at(&SourceLocation::from_spanned(x.span, source.clone()));
                        out_list = TrackedSexpr::cons(
                            Self::from_spanned(x, source.clone()),
                            out_list,
                            src,
                        );
                    }
                }
                out_list
            }
            PS::Vector(l) => {
                let items: Vec<_> = l
                    .into_iter()
                    .map(|i| Self::from_spanned(i, source.clone()))
                    .collect();
                TrackedSexpr::new(
                    Sexpr::Vector(items.into()),
                    SourceLocation::from_spanned(se.span, source),
                )
            }
            x => unimplemented!("SpannedSexpr::{:?} --> TrackesSexpr", x),
        }
    }

    pub fn with_src(mut self, src: SourceLocation) -> Self {
        self.src = src;
        self
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

    pub fn undefined() -> Self {
        TrackedSexpr {
            sexpr: Sexpr::Undefined,
            src: SourceLocation::NoSource,
        }
    }

    pub fn nil(src: SourceLocation) -> Self {
        TrackedSexpr {
            sexpr: Sexpr::Nil,
            src,
        }
    }

    pub fn symbol(s: impl Into<Symbol>, src: SourceLocation) -> Self {
        TrackedSexpr {
            sexpr: Sexpr::Symbol(s.into()),
            src,
        }
    }

    pub fn escaped_string(s: &str) -> Self {
        let mut buffer = Vec::with_capacity(s.len());
        let mut bytes = s.bytes();
        while let Some(b) = bytes.next() {
            let x = if b == b'\\' {
                match bytes.next() {
                    Some(b'a') => 0x07,
                    Some(b'b') => 0x08,
                    Some(b't') => 0x09,
                    Some(b'n') => 0x0A,
                    Some(b'r') => 0x0D,
                    Some(b'"') => 0x22,
                    Some(b'\\') => 0x5C,
                    Some(b'|') => 0x7C,
                    Some(b'x') => unimplemented!(),
                    Some(x) => {
                        eprintln!("Warning: invalid escape sequence \\{}", x as char);
                        buffer.push(b'\\');
                        x
                    }
                    None => b'\\',
                }
            } else {
                b
            };
            buffer.push(x);
        }

        TrackedSexpr {
            sexpr: Sexpr::String(String::from_utf8(buffer).unwrap().into()),
            src: SourceLocation::NoSource,
        }
    }

    pub fn list(data: Vec<TrackedSexpr>, src: SourceLocation) -> Self {
        let mut l = TrackedSexpr::nil(src.last_char());
        for x in data.into_iter().rev() {
            let src = l.src.start_at(&x.src);
            l = TrackedSexpr::cons(x, l, src);
        }
        l
    }

    pub fn cons(car: Self, cdr: Self, src: SourceLocation) -> Self {
        TrackedSexpr {
            sexpr: Sexpr::Pair(Box::new((car, cdr))),
            src,
        }
    }

    pub fn car(&self) -> Result<&Self> {
        match &self.sexpr {
            Sexpr::Pair(p) => Ok(&p.0),
            _ => Err(Error::at_expr(
                TypeError::NoPair(Scm::string(format!("{}", self))),
                self,
            )),
        }
    }

    pub fn cdr(&self) -> Result<&Self> {
        match &self.sexpr {
            Sexpr::Pair(p) => Ok(&p.1),
            _ => Err(Error::at_expr(
                TypeError::NoPair(Scm::string(format!("{}", self))),
                self,
            )),
        }
    }

    pub fn decons(self) -> std::result::Result<(Self, Self), Self> {
        match self.sexpr {
            Sexpr::Pair(p) => Ok((p.0, p.1)),
            _ => Err(self),
        }
    }

    pub fn at(&self, idx: usize) -> Result<&Self> {
        if idx == 0 {
            self.car()
        } else {
            self.cdr().and_then(|cdr| cdr.at(idx - 1))
        }
    }

    pub fn is_null(&self) -> bool {
        match self.sexpr {
            Sexpr::Nil => true,
            _ => false,
        }
    }

    pub fn is_atom(&self) -> bool {
        match &self.sexpr {
            Sexpr::Pair(_) => false,
            _ => true,
        }
    }

    pub fn is_identifier(&self) -> bool {
        self.is_symbol() || self.is_alias()
    }

    pub fn identifier_name(&self) -> Option<Symbol> {
        match &self.sexpr {
            Sexpr::Symbol(s) => Some(*s),
            Sexpr::SyntacticClosure(sc) => sc.alias_name(),
            _ => None,
        }
    }

    pub fn is_alias(&self) -> bool {
        self.as_alias().is_some()
    }

    pub fn as_alias(&self) -> Option<&Rc<SyntacticClosure>> {
        match &self.sexpr {
            Sexpr::SyntacticClosure(sc) if sc.is_alias() => Some(sc),
            _ => None,
        }
    }

    pub fn is_symbol(&self) -> bool {
        self.as_symbol().is_ok()
    }

    pub fn as_symbol(&self) -> Result<&Symbol> {
        match &self.sexpr {
            Sexpr::Symbol(s) => Ok(s),
            Sexpr::SyntacticClosure(sc) => sc.sexpr().as_symbol(),
            _ => Err(Error::at_expr(TypeError::NoSymbol, self)),
        }
    }

    pub fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }

    pub fn as_pair(&self) -> Option<(&Self, &Self)> {
        match &self.sexpr {
            Sexpr::Pair(p) => Some((&p.0, &p.1)),
            _ => None,
        }
    }

    pub fn scan<E>(
        &self,
        mut f: impl FnMut(&Self) -> std::result::Result<(), E>,
    ) -> std::result::Result<&Self, E> {
        let mut x = self;
        while x.is_pair() {
            f(x.car().unwrap())?;
            x = x.cdr().unwrap();
        }
        Ok(x)
    }

    pub fn scan_improper<E>(
        &self,
        mut f: impl FnMut(&Self, bool) -> std::result::Result<(), E>,
    ) -> std::result::Result<&Self, E> {
        let mut x = self;
        while x.is_pair() {
            f(x.car().unwrap(), false)?;
            x = x.cdr().unwrap();
        }
        f(x, true)?;
        Ok(x)
    }

    pub fn contains(&self, x: &Sexpr) -> bool {
        match &self.sexpr {
            Sexpr::Vector(v) => v.iter().find(|item| &item.sexpr == x).is_some(),
            Sexpr::Pair(p) if &p.0.sexpr == x => true,
            Sexpr::Pair(p) => p.1.contains(x),
            _ => false,
        }
    }

    pub fn list_len(&self) -> usize {
        match &self.sexpr {
            Sexpr::Pair(p) => 1 + p.1.list_len(),
            _ => 0,
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
            Cons(p) => {
                let (car, cdr) = p.into_pair();
                Sexpr::Pair(Box::new((car.into(), cdr.into())))
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

impl From<Sexpr> for TrackedSexpr {
    fn from(sexpr: Sexpr) -> Self {
        TrackedSexpr {
            sexpr,
            src: SourceLocation::NoSource,
        }
    }
}

impl PartialEq<str> for TrackedSexpr {
    fn eq(&self, other: &str) -> bool {
        match &self.sexpr {
            Sexpr::Symbol(s) => s == other,
            Sexpr::String(s) if other.starts_with('"') && other.ends_with('"') => {
                **s == other[1..other.len() - 1]
            }
            Sexpr::SyntacticClosure(sc) => sc.sexpr() == other,
            _ => false,
        }
    }
}

impl PartialEq for TrackedSexpr {
    fn eq(&self, other: &Self) -> bool {
        self.sexpr == other.sexpr
    }
}
