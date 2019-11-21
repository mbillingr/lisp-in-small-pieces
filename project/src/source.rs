use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum SourceLocation {
    NoSource,
    Span(Source, usize, usize),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Source {
    content: Rc<String>,
    file: Option<PathBuf>,
}

impl SourceLocation {
    pub fn last_char(&self) -> Self {
        match self {
            SourceLocation::NoSource => SourceLocation::NoSource,
            SourceLocation::Span(src, start, end) => {
                SourceLocation::Span(src.clone(), end - 1, *end)
            }
        }
    }

    pub fn start_at(self, other: &Self) -> Self {
        use SourceLocation::*;
        match (self, other) {
            (Span(ref src, _, _), Span(osrc, _, _)) if src != osrc => {}
            (Span(src, _, end), &Span(_, start, _)) => return Span(src, start, end),
            (NoSource, NoSource) => return NoSource,
            _ => {}
        }
        panic!("detected attempt to combine spans from different sources")
    }
}
