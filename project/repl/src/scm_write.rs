use crate::scm::Scm;
use std::cell::Cell;
use std::fmt::Display;
use std::marker::PhantomData;

pub struct ScmDisplay(Scm);
pub struct ScmWriteSimple(Scm);

pub struct ScmWriteShared<T> {
    value: Scm,
    shared: Vec<Scm>,
    _atom_writer: PhantomData<T>,
}

impl ScmDisplay {
    pub fn new(scm: Scm) -> Self {
        ScmDisplay(scm)
    }
}

impl From<Scm> for ScmDisplay {
    fn from(scm: Scm) -> Self {
        ScmDisplay(scm)
    }
}

impl ScmWriteSimple {
    pub fn new(scm: Scm) -> Self {
        ScmWriteSimple(scm)
    }
}

impl From<Scm> for ScmWriteSimple {
    fn from(scm: Scm) -> Self {
        ScmWriteSimple(scm)
    }
}

impl<T: Display + From<Scm>> ScmWriteShared<T> {
    pub fn new_shared(scm: Scm) -> Self {
        let mut seen = vec![];
        Self::walk_data(scm, &mut seen);

        let shared: Vec<_> = seen
            .into_iter()
            .filter_map(
                |(obj, count)| {
                    if count.get() < 2 {
                        None
                    } else {
                        Some(obj)
                    }
                },
            )
            .collect();

        ScmWriteShared {
            value: scm,
            shared,
            _atom_writer: PhantomData,
        }
    }

    pub fn new_cyclic(scm: Scm) -> Self {
        let mut shared = vec![];
        Self::walk_cycles(scm, &mut vec![], &mut shared);
        ScmWriteShared {
            value: scm,
            shared,
            _atom_writer: PhantomData,
        }
    }

    pub fn new_display(scm: Scm) -> Self {
        let mut shared = vec![];
        Self::walk_cycles(scm, &mut vec![], &mut shared);
        ScmWriteShared {
            value: scm,
            shared,
            _atom_writer: PhantomData,
        }
    }

    fn write(
        &self,
        scm: Scm,
        labels: &mut Vec<Scm>,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        match scm {
            Scm::Vector(_) | Scm::Pair(_)
                if self.shared.iter().find(|obj| obj.ptr_eq(&scm)).is_some() =>
            {
                match labels.iter().position(|obj| obj.ptr_eq(&scm)) {
                    Some(p) => return write!(f, "#{}#", p),
                    None => {
                        let p = labels.len();
                        labels.push(scm);
                        write!(f, "#{}=", p)?;
                    }
                }
            }
            _ => {}
        }

        match scm {
            Scm::Vector(v) => {
                write!(f, "#(")?;
                let mut items = v.iter();
                if let Some(x) = items.next() {
                    self.write(x.get(), labels, f)?;
                    for x in items {
                        write!(f, " ")?;
                        self.write(x.get(), labels, f)?;
                    }
                }
                write!(f, ")")
            }
            Scm::Pair(p) if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("quote") => {
                write!(f, "'")?;
                self.write(p.1.get().car().unwrap(), labels, f)
            }
            Scm::Pair(p)
                if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("quasiquote") =>
            {
                write!(f, "`")?;
                self.write(p.1.get().car().unwrap(), labels, f)
            }
            Scm::Pair(p) if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("unquote") => {
                write!(f, ",")?;
                self.write(p.1.get().car().unwrap(), labels, f)
            }
            Scm::Pair(p)
                if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("unquote-splicing") =>
            {
                write!(f, ",@")?;
                self.write(p.1.get().car().unwrap(), labels, f)
            }
            Scm::Pair(p) => {
                write!(f, "(")?;
                self.write(p.0.get(), labels, f)?;
                let mut cdr = p.1.get();
                loop {
                    if self.shared.iter().find(|obj| obj.ptr_eq(&cdr)).is_some() {
                        write!(f, " . ")?;
                        self.write(cdr, labels, f)?;
                        break;
                    }
                    match cdr {
                        Scm::Nil => break,
                        Scm::Pair(q)
                            if [Some("quote"), Some("quasiquote"), Some("unquote")]
                                .contains(&q.0.get().as_symbol().map(|s| s.as_str()).ok()) =>
                        {
                            write!(f, " . ")?;
                            self.write(cdr, labels, f)?;
                            break;
                        }
                        Scm::Pair(q) => {
                            write!(f, " ")?;
                            self.write(q.0.get(), labels, f)?;
                            cdr = q.1.get();
                        }
                        x => {
                            write!(f, " . ")?;
                            self.write(x, labels, f)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            _ => T::from(scm).fmt(f),
        }
    }

    fn walk_data(scm: Scm, seen: &mut Vec<(Scm, Cell<usize>)>) {
        match scm {
            Scm::Vector(_) | Scm::Pair(_) => match seen.iter().find(|(obj, _)| obj.ptr_eq(&scm)) {
                None => {
                    seen.push((scm, Cell::new(1)));

                    match scm {
                        Scm::Pair(p) => {
                            Self::walk_data(p.0.get(), seen);
                            Self::walk_data(p.1.get(), seen);
                        }
                        Scm::Vector(v) => {
                            for x in v {
                                Self::walk_data(x.get(), seen);
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                Some((_, count)) => count.set(1 + count.get()),
            },
            _ => {}
        }
    }

    fn walk_cycles(scm: Scm, ancestors: &mut Vec<Scm>, seen: &mut Vec<Scm>) {
        match scm {
            Scm::Vector(_) | Scm::Pair(_) => {
                if ancestors.iter().find(|obj| obj.ptr_eq(&scm)).is_some() {
                    if seen.iter().find(|obj| obj.ptr_eq(&scm)).is_none() {
                        seen.push(scm);
                    }
                } else {
                    ancestors.push(scm);

                    match scm {
                        Scm::Pair(p) => {
                            Self::walk_cycles(p.0.get(), ancestors, seen);
                            Self::walk_cycles(p.1.get(), ancestors, seen);
                        }
                        Scm::Vector(v) => {
                            for x in v {
                                Self::walk_cycles(x.get(), ancestors, seen);
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                ancestors.pop();
            }
            _ => {}
        }
    }
}

impl Display for ScmDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Scm::Undefined => write!(f, "<undefined>"),
            Scm::Uninitialized => write!(f, "<uninitialized>"),
            Scm::Nil => write!(f, "()"),
            Scm::True => write!(f, "#t"),
            Scm::False => write!(f, "#f"),
            Scm::Eof => write!(f, "<eof>"),
            Scm::Int(i) => write!(f, "{}", i),
            Scm::Float(x) => write!(f, "{}", x),
            Scm::Char(c) => write!(f, "{}", c),
            Scm::Symbol(s) => write!(f, "{}", s),
            Scm::String(s) => write!(f, "{}", s),
            Scm::Bytevector(v) => write_bytevector(v, f),
            Scm::Vector(v) => {
                write!(f, "#(")?;
                let mut items = v.iter();
                if let Some(x) = items.next() {
                    write!(f, "{}", x.get().display())?;
                    for x in items {
                        write!(f, " {}", x.get().display())?;
                    }
                }
                write!(f, ")")
            }
            Scm::Pair(p) if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("quote") => {
                write!(f, "'{}", p.1.get().car().unwrap().display())
            }
            Scm::Pair(p)
                if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("quasiquote") =>
            {
                write!(f, "`{}", p.1.get().car().unwrap().display())
            }
            Scm::Pair(p) if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("unquote") => {
                write!(f, ",{}", p.1.get().car().unwrap().display())
            }
            Scm::Pair(p)
                if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("unquote-splicing") =>
            {
                write!(f, ",@{}", p.1.get().car().unwrap().display())
            }
            Scm::Pair(p) => {
                write!(f, "({}", p.0.get().display())?;
                let mut cdr = p.1.get();
                loop {
                    match cdr {
                        Scm::Nil => break,
                        Scm::Pair(q)
                            if [Some("quote"), Some("quasiquote"), Some("unquote")]
                                .contains(&q.0.get().as_symbol().map(|s| s.as_str()).ok()) =>
                        {
                            write!(f, " . {}", cdr.display())?;
                            break;
                        }
                        Scm::Pair(q) => {
                            write!(f, " {}", q.0.get().display())?;
                            cdr = q.1.get();
                        }
                        x => {
                            write!(f, " . {}", x.display())?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Scm::Closure(cls) => write!(f, "<closure {:p}>", cls),
            Scm::Primitive(prim) => write!(f, "<primitive {:?}>", prim),
            Scm::Continuation(cnt) => write!(f, "<continuation {:?}>", cnt),
            Scm::ExitProc(cnt) => write!(f, "<exit-procedure {:?}>", cnt),
            Scm::Port(p) => write!(f, "<port {:?}>", p),
            Scm::Cell(c) => write!(f, "{}", c.get().display()),
            Scm::Rust(o) => write!(f, "<rust object {:p}>", *o),
            Scm::Error(e) => write!(f, "<error object {:?}>", e),
        }
    }
}

impl<T: Display + From<Scm>> Display for ScmWriteShared<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut labels = vec![];
        self.write(self.value, &mut labels, f)
    }
}

impl Display for ScmWriteSimple {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Scm::Undefined => write!(f, "<undefined>"),
            Scm::Uninitialized => write!(f, "<uninitialized>"),
            Scm::Nil => write!(f, "'()"),
            Scm::True => write!(f, "#t"),
            Scm::False => write!(f, "#f"),
            Scm::Eof => write!(f, "<eof>"),
            Scm::Int(i) => write!(f, "{}", i),
            Scm::Float(x) => write!(f, "{}", x),
            Scm::Char(c) => write!(f, "#\\{}", c),
            Scm::Symbol(s) => write!(f, "{}", s),
            Scm::String(s) => write!(f, "{:?}", s),
            Scm::Bytevector(v) => write_bytevector(v, f),
            Scm::Vector(v) => {
                write!(f, "#(")?;
                let mut items = v.iter();
                if let Some(x) = items.next() {
                    write!(f, "{}", x.get().write_simple())?;
                    for x in items {
                        write!(f, " {}", x.get().write_simple())?;
                    }
                }
                write!(f, ")")
            }
            Scm::Pair(p) if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("quote") => {
                write!(f, "'{}", p.1.get().car().unwrap().write_simple())
            }
            Scm::Pair(p)
                if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("quasiquote") =>
            {
                write!(f, "`{}", p.1.get().car().unwrap().write_simple())
            }
            Scm::Pair(p) if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("unquote") => {
                write!(f, ",{}", p.1.get().car().unwrap().write_simple())
            }
            Scm::Pair(p)
                if p.0.get().as_symbol().map(|s| s.as_str()).ok() == Some("unquote-splicing") =>
            {
                write!(f, ",@{}", p.1.get().car().unwrap().write_simple())
            }
            Scm::Pair(p) => {
                write!(f, "({}", p.0.get().write_simple())?;
                let mut cdr = p.1.get();
                loop {
                    match cdr {
                        Scm::Nil => break,
                        Scm::Pair(q)
                            if [Some("quote"), Some("quasiquote"), Some("unquote")]
                                .contains(&q.0.get().as_symbol().map(|s| s.as_str()).ok()) =>
                        {
                            write!(f, " . {}", cdr.write_simple())?;
                            break;
                        }
                        Scm::Pair(q) => {
                            write!(f, " {}", q.0.get().write_simple())?;
                            cdr = q.1.get();
                        }
                        x => {
                            write!(f, " . {}", x.write_simple())?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Scm::Closure(cls) => write!(f, "<closure {:p}>", cls),
            Scm::Primitive(prim) => write!(f, "<primitive {:?}>", prim),
            Scm::Continuation(cnt) => write!(f, "<continuation {:?}>", cnt),
            Scm::ExitProc(cnt) => write!(f, "<exit-procedure {:?}>", cnt),
            Scm::Port(p) => write!(f, "<port {:?}>", p),
            Scm::Cell(c) => write!(f, "{}", c.get().display()),
            Scm::Rust(o) => write!(f, "<rust object {:p}>", *o),
            Scm::Error(e) => write!(f, "<error object {:?}>", e),
        }
    }
}

fn write_bytevector(v: &[u8], f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "#u8(")?;
    let mut items = v.iter();
    if let Some(x) = items.next() {
        write!(f, "{}", x)?;
        for x in items {
            write!(f, " {}", x)?;
        }
    }
    write!(f, ")")
}
