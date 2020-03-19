pub trait Spanned<'a> {
    fn span(&self) -> &Span<'a>;
    fn span_mut(&mut self) -> &mut Span<'a>;
}

impl<'a> Spanned<'a> for Span<'a> {
    fn span(&self) -> &Span<'a> {
        self
    }
    fn span_mut(&mut self) -> &mut Span<'a> {
        self
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Span<'a> {
    pub text: &'a str,
    pub start: usize,
    pub end: usize,
}

impl<'a> std::ops::Deref for Span<'a> {
    type Target = str;
    fn deref(&self) -> &str {
        &self.text[self.start..self.end]
    }
}

impl<'a> Default for Span<'a> {
    fn default() -> Span<'a> {
        Span {
            text: "",
            start: 0,
            end: 0,
        }
    }
}

impl<'a> Span<'a> {
    pub fn new(text: &'a str) -> Self {
        Span {
            text,
            start: 0,
            end: text.len(),
        }
    }

    pub fn range(from: Span<'a>, to: Span<'a>) -> Self {
        assert_eq!(from.text, to.text);
        Span {
            text: from.text,
            start: from.start,
            end: to.end,
        }
    }

    pub fn empty(&self) -> bool {
        self.end == self.start
    }

    pub fn split(&self, n: usize) -> (Self, Self) {
        let first = Span {
            text: self.text,
            start: self.start,
            end: self.start + n,
        };
        let second = Span {
            text: self.text,
            start: self.start + n,
            end: self.end,
        };
        (first, second)
    }

    pub fn as_str(&self) -> &str {
        &self.text[self.start..self.end]
    }
}
