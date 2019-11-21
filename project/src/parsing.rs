use crate::sexpr::TrackedSexpr;
use crate::source::{Source, Span};
use std::rc::Rc;
use std::path::PathBuf;
use std::io::Read;
use nom::{IResult, character::complete::{digit0, }};

pub fn parse_string(src: impl Into<String>) -> TrackedSexpr {
    parse_source(src.into().into())
}

pub fn parse_file(file: impl Into<PathBuf>) -> TrackedSexpr {
    parse_source(Source::from_file(file).unwrap())
}

pub fn parse_source(src: Source) -> TrackedSexpr {
    unimplemented!()
}

pub fn parse_number(input: Span) {
    digit0(input)
}