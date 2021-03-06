use crate::parsing;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::rc::Rc;

#[derive(Clone)]
pub enum SourceLocation {
    NoSource,
    Span(Span),
}

#[derive(Clone)]
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

impl Source {
    pub fn loc(self, start: usize, end: usize) -> SourceLocation {
        assert!(start <= self.content.len());
        assert!(end <= self.content.len());
        SourceLocation::Span(Span {
            src: self,
            start,
            end,
        })
    }
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

    pub fn join(first: &Self, second: &Self) -> Self {
        assert!(first.is_compatible(second));
        Span {
            src: first.src.clone(),
            start: first.start.min(second.start),
            end: second.end.max(second.end),
        }
    }

    pub fn is_compatible(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.src.content, &other.src.content)
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let first_line = self.src.line_number(self.start);
        let last_line = self.src.line_number(self.end);

        if first_line == last_line {
            let (line, line_start) = first_line;
            let span_length = self.end - self.start;
            writeln!(f, "{:>4}   {}", line + 1, self.src.extract_line(line))?;
            write!(
                f,
                "      {: >2$}{:^>3$}",
                "",
                "",
                self.start - line_start,
                span_length
            )
        } else {
            let (first, first_start) = first_line;
            let first_text = self.src.extract_line(first);
            writeln!(f, "{:>4}   {}", first + 1, first_text)?;
            writeln!(
                f,
                "       {: >2$}{:^>3$}...",
                "",
                "",
                self.start - first_start,
                first_text.len() + first_start - self.start,
            )?;

            let (last, last_start) = last_line;
            let last_text = self.src.extract_line(last);
            writeln!(f, "{:>4}   {}", last + 1, last_text)?;
            writeln!(
                f,
                "    ...{:^>1$}",
                "",
                (last_start + last_text.len()).max(self.end) - self.end,
            )
        }
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
            (NoSource, _) | (_, NoSource) => return NoSource,
        }
        panic!(
            "detected attempt to combine spans from different sources:\n    {:?}\n    {:?}",
            self, other
        )
    }

    pub fn join(&self, other: &Self) -> Self {
        use SourceLocation::*;
        match (self, other) {
            (Span(s1), Span(s2)) if !s1.is_compatible(s2) => {}
            (Span(s1), Span(s2)) => return Span(self::Span::join(s1, s2)),
            (NoSource, _) | (_, NoSource) => return NoSource,
        }
        panic!(
            "detected attempt to combine spans from different sources:\n    {:?}\n    {:?}",
            self, other
        )
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
    pub fn from_file(file: &Path) -> std::io::Result<Self> {
        let mut f = File::open(file)?;
        let mut src = String::new();
        f.read_to_string(&mut src)?;
        Ok(Source {
            content: Rc::new(src),
            file: Some(file.to_owned()),
        })
    }

    // return line number and byte offset into line for given byte offset into full text
    pub fn line_number(&self, pos: usize) -> (usize, usize) {
        let mut count = 0;
        let mut line_pos = 0;
        for (i, _) in self
            .content
            .bytes()
            .enumerate()
            .take(pos)
            .filter(|&(_, ch)| ch == b'\n')
        {
            count += 1;
            line_pos = i;
        }
        (count, line_pos)
    }

    pub fn extract_line(&self, n: usize) -> &str {
        self.content.lines().take(n + 1).last().unwrap()
    }
}

impl std::fmt::Debug for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SourceLocation::NoSource => write!(f, "*no source avialable*"),
            SourceLocation::Span(span) => span.fmt(f),
        }
    }
}
impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "\n{}", self)
        } else {
            write!(f, "{}", &self.src.content[self.start..self.end])
        }
    }
}

impl From<Source> for Span {
    fn from(src: Source) -> Self {
        Span {
            start: 0,
            end: src.content.len(),
            src,
        }
    }
}

impl From<Span> for SourceLocation {
    fn from(span: Span) -> Self {
        SourceLocation::Span(span)
    }
}

impl From<Source> for SourceLocation {
    fn from(src: Source) -> Self {
        SourceLocation::Span(src.into())
    }
}
