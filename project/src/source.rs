use crate::parsing;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum SourceLocation {
    NoSource,
    Span(Span),
}

#[derive(Debug, Clone)]
pub struct Span {
    src: Source,
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Source {
    pub content: Rc<String>,
    pub file: Option<PathBuf>,
}

impl Span {
    pub fn last_char(&self) -> Self {
        Span {
            src: self.src.clone(),
            start: self.end - 1,
            end: self.end,
        }
    }

    pub fn unite(first: &Self, second: &Self) -> Self {
        assert!(first.is_compatible(second));
        Span {
            src: first.src.clone(),
            start: first.start,
            end: second.end,
        }
    }

    pub fn is_compatible(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.src.content, &other.src.content)
    }
}

impl SourceLocation {
    pub fn from_spanned(span: parsing::Span, src: Source) -> Self {
        SourceLocation::Span(Span {
            src,
            start: span.start,
            end: span.end,
        })
    }

    pub fn last_char(&self) -> Self {
        match self {
            SourceLocation::NoSource => SourceLocation::NoSource,
            SourceLocation::Span(span) => SourceLocation::Span(span.last_char()),
        }
    }

    pub fn start_at(&self, other: &Self) -> Self {
        use SourceLocation::*;
        match (self, other) {
            (Span(s1), Span(s2)) if !s1.is_compatible(s2) => {}
            (Span(s1), Span(s2)) => return Span(self::Span::unite(s2, s1)),
            (NoSource, NoSource) => return NoSource,
            _ => {}
        }
        panic!("detected attempt to combine spans from different sources")
    }
}

impl<T: Into<String>> From<T> for Source {
    fn from(s: T) -> Self {
        Source {
            content: Rc::new(s.into()),
            file: None,
        }
    }
}

impl Source {
    pub fn from_file(file: impl Into<PathBuf>) -> std::io::Result<Self> {
        let file = file.into();
        let mut f = File::open(&file).unwrap();
        let mut src = String::new();
        f.read_to_string(&mut src).unwrap();
        Ok(Source {
            content: Rc::new(src),
            file: Some(file),
        })
    }
}
