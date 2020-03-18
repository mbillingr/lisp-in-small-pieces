use crate::error::{Error, Result, RuntimeError};
use crate::parsing::{parse_sexpr, ParseErrorKind, Span};
use crate::scm::Scm;
use std::cell::RefCell;
use std::fs::File;
use std::io::{stderr, stdin, stdout, BufRead, BufReader, BufWriter, Write};
use std::mem::size_of;

pub struct SchemePort {
    port: RefCell<PortState>,
}

impl std::fmt::Debug for SchemePort {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<Port {:p}>", self)
    }
}

impl SchemePort {
    pub fn std_input() -> Self {
        SchemePort {
            port: RefCell::new(PortState::Input(Box::new(BufReader::new(stdin())))),
        }
    }

    pub fn std_output() -> Self {
        SchemePort {
            port: RefCell::new(PortState::Output(Box::new(stdout()))),
        }
    }

    pub fn std_error() -> Self {
        SchemePort {
            port: RefCell::new(PortState::Output(Box::new(stderr()))),
        }
    }

    pub fn file_input(filename: &str) -> Result<Self> {
        let f = File::open(filename)?;
        Ok(SchemePort {
            port: RefCell::new(PortState::Input(Box::new(BufReader::new(f)))),
        })
    }

    pub fn file_output(filename: &str) -> Result<Self> {
        let f = File::create(filename)?;
        Ok(SchemePort {
            port: RefCell::new(PortState::Output(Box::new(BufWriter::new(f)))),
        })
    }

    pub fn string_input(s: &'static str) -> Self {
        SchemePort {
            port: RefCell::new(PortState::Input(Box::new(s.as_bytes()))),
        }
    }

    pub fn bytes_input(s: &'static [u8]) -> Self {
        SchemePort {
            port: RefCell::new(PortState::Input(Box::new(s))),
        }
    }

    pub fn bytes_output() -> Self {
        SchemePort {
            port: RefCell::new(PortState::Buffer(vec![])),
        }
    }

    pub fn is_open(&self) -> bool {
        match &*self.port.borrow() {
            PortState::Closed => false,
            _ => true,
        }
    }

    pub fn close(&self) -> Result<()> {
        let mut p = self.port.borrow_mut();
        match &mut *p {
            PortState::Output(port) => port.flush_output()?,
            _ => {}
        }
        *p = PortState::Closed;
        Ok(())
    }

    pub fn clone_data(&self) -> Result<Vec<u8>> {
        match &*self.port.borrow() {
            PortState::Buffer(data) => Ok(data.clone()),
            _ => Err(RuntimeError::WrongPortKind.into()),
        }
    }

    fn with_input_port<T>(&self, func: impl FnOnce(&mut dyn InputPort) -> Result<T>) -> Result<T> {
        match &mut *self.port.borrow_mut() {
            PortState::Input(p) => func(p.as_mut()),
            PortState::Output(_) | PortState::Buffer(_) => Err(RuntimeError::WrongPortKind.into()),
            PortState::Closed => Err(RuntimeError::ClosedPort.into()),
        }
    }

    fn with_output_port<T>(
        &self,
        func: impl FnOnce(&mut dyn OutputPort) -> Result<T>,
    ) -> Result<T> {
        match &mut *self.port.borrow_mut() {
            PortState::Output(p) => func(p.as_mut()),
            PortState::Buffer(p) => func(&mut p.as_mut_slice()),
            PortState::Input(_) => Err(RuntimeError::WrongPortKind.into()),
            PortState::Closed => Err(RuntimeError::ClosedPort.into()),
        }
    }
}

impl SchemePort {
    pub fn is_input_port(&self) -> bool {
        match &*self.port.borrow() {
            PortState::Input(_) => true,
            _ => false,
        }
    }

    pub fn is_output_port(&self) -> bool {
        match &*self.port.borrow() {
            PortState::Output(_) => true,
            _ => false,
        }
    }

    pub fn read(&self) -> Result<Scm> {
        self.with_input_port(|p| read_sexpr(p))
    }

    pub fn read_char(&self) -> Result<Scm> {
        self.with_input_port(|p| p.read_char())
            .map(convert_optional_value_to_scm)
    }
    pub fn peek_char(&self) -> Result<Scm> {
        self.with_input_port(|p| p.peek_char())
            .map(convert_optional_value_to_scm)
    }
    pub fn read_line(&self) -> Result<Scm> {
        self.with_input_port(|p| p.read_line())
            .map(convert_optional_value_to_scm)
    }
    pub fn read_string(&self, k: usize) -> Result<Scm> {
        self.with_input_port(|p| p.read_string(k))
            .map(convert_optional_value_to_scm)
    }
    pub fn read_u8(&self) -> Result<Scm> {
        self.with_input_port(|p| p.read_u8())
            .map(convert_optional_value_to_scm)
    }
    pub fn peek_u8(&self) -> Result<Scm> {
        self.with_input_port(|p| p.peek_u8())
            .map(convert_optional_value_to_scm)
    }
    pub fn read_bytevector(&self, k: usize) -> Result<Scm> {
        self.with_input_port(|p| p.read_bytevector(k))
            .map(convert_optional_value_to_scm)
    }
    pub fn read_into_bytevector(&self, start: usize, end: usize, vec: &mut [u8]) -> Result<Scm> {
        self.with_input_port(|p| p.read_into_bytevector(start, end, vec))
            .map(convert_optional_value_to_scm)
    }

    pub fn write_char(&self, ch: char) -> Result<()> {
        self.with_output_port(|p| p.write_char(ch))
    }
    pub fn write_string(&self, s: &str, start: usize, end: usize) -> Result<()> {
        self.with_output_port(|p| p.write_string(s, start, end))
    }
    pub fn write_u8(&self, x: u8) -> Result<()> {
        self.with_output_port(|p| p.write_u8(x))
    }
    pub fn write_bytevector(&self, v: &[u8], start: usize, end: usize) -> Result<()> {
        self.with_output_port(|p| p.write_bytevector(&v[start..end]))
    }
    pub fn flush_output(&self) -> Result<()> {
        self.with_output_port(|p| p.flush_output())
    }

    pub fn write(&self, x: Scm) -> Result<()> {
        self.with_output_port(|p| p.write(x))
    }
    pub fn write_shared(&self, x: Scm) -> Result<()> {
        self.with_output_port(|p| p.write_shared(x))
    }
    pub fn write_simple(&self, x: Scm) -> Result<()> {
        self.with_output_port(|p| p.write_simple(x))
    }
    pub fn display(&self, x: Scm) -> Result<()> {
        self.with_output_port(|p| p.display(x))
    }
}

fn convert_optional_value_to_scm<T: Into<Scm>>(value: Option<T>) -> Scm {
    match value {
        None => Scm::Eof,
        Some(x) => x.into(),
    }
}

enum PortState {
    Input(Box<dyn InputPort>),
    Output(Box<dyn OutputPort>),
    Buffer(Vec<u8>),
    Closed,
}

pub trait InputPort {
    fn read_char(&mut self) -> Result<Option<char>>;
    fn peek_char(&mut self) -> Result<Option<char>>;
    fn read_line(&mut self) -> Result<Option<String>>;
    fn read_string(&mut self, k: usize) -> Result<Option<String>>;
    fn read_u8(&mut self) -> Result<Option<u8>>;
    fn peek_u8(&mut self) -> Result<Option<u8>>;
    fn read_bytevector(&mut self, k: usize) -> Result<Option<Vec<u8>>>;
    fn read_into_bytevector(
        &mut self,
        start: usize,
        end: usize,
        vec: &mut [u8],
    ) -> Result<Option<usize>>;
}

pub trait OutputPort {
    fn write(&mut self, x: Scm) -> Result<()>;
    fn write_shared(&mut self, x: Scm) -> Result<()>;
    fn write_simple(&mut self, x: Scm) -> Result<()>;
    fn display(&mut self, x: Scm) -> Result<()>;
    fn write_char(&mut self, ch: char) -> Result<()>;
    fn write_string(&mut self, s: &str, start: usize, end: usize) -> Result<()>;
    fn write_u8(&mut self, x: u8) -> Result<()>;
    fn write_bytevector(&mut self, v: &[u8]) -> Result<()>;
    fn flush_output(&mut self) -> Result<()>;
}

impl<T: BufRead> InputPort for T {
    fn read_char(&mut self) -> Result<Option<char>> {
        let och = self.peek_char()?;
        match och {
            Some(ch) => self.consume(ch.len_utf8()),
            None => {}
        }
        Ok(och)
    }

    fn peek_char(&mut self) -> Result<Option<char>> {
        let buf = self.fill_buf()?;
        let s = if buf.len() >= size_of::<char>() {
            std::str::from_utf8(&buf[..size_of::<char>()])?
        } else {
            std::str::from_utf8(buf)?
        };
        Ok(s.chars().next())
    }

    fn read_line(&mut self) -> Result<Option<String>> {
        let mut buf = String::new();
        let n = BufRead::read_line(self, &mut buf)?;
        if n == 0 {
            Ok(None)
        } else {
            Ok(Some(buf))
        }
    }

    fn read_string(&mut self, k: usize) -> Result<Option<String>> {
        let buf = (0..k)
            .map(|_| self.read_char())
            .take_while(|r| match r {
                Err(_) => false,
                Ok(None) => false,
                Ok(_) => true,
            })
            .map(|r| r.map(|x| x.unwrap()))
            .collect::<Result<String>>()?;
        if buf.len() == 0 && k > 0 {
            Ok(None)
        } else {
            Ok(Some(buf))
        }
    }

    fn read_u8(&mut self) -> Result<Option<u8>> {
        let ou = self.peek_u8()?;
        if ou.is_some() {
            self.consume(1);
        }
        Ok(ou)
    }

    fn peek_u8(&mut self) -> Result<Option<u8>> {
        let buf = self.fill_buf()?;
        if buf.is_empty() {
            Ok(None)
        } else {
            Ok(Some(buf[0]))
        }
    }

    fn read_bytevector(&mut self, k: usize) -> Result<Option<Vec<u8>>> {
        let mut buf = vec![0; k];
        match self.read_into_bytevector(0, k, buf.as_mut_slice())? {
            None => return Ok(None),
            Some(n) if n as usize == k => {}
            Some(n) => buf.truncate(n),
        }
        Ok(Some(buf.into()))
    }

    fn read_into_bytevector(
        &mut self,
        start: usize,
        end: usize,
        vec: &mut [u8],
    ) -> Result<Option<usize>> {
        if end <= start {
            return Ok(Some(0));
        }

        let vec = &mut vec[start..end];
        self.read(vec)
            .map(|n| match n {
                0 => None,
                n => Some(n),
            })
            .map_err(Error::from)
    }
}

impl<T: Write> OutputPort for T {
    fn write(&mut self, x: Scm) -> Result<()> {
        Ok(write!(self, "{}", x.write())?)
    }

    fn write_shared(&mut self, x: Scm) -> Result<()> {
        Ok(write!(self, "{}", x.write_shared())?)
    }

    fn write_simple(&mut self, x: Scm) -> Result<()> {
        Ok(write!(self, "{}", x.write_simple())?)
    }

    fn display(&mut self, x: Scm) -> Result<()> {
        Ok(write!(self, "{}", x.display())?)
    }

    fn write_char(&mut self, ch: char) -> Result<()> {
        Ok(write!(self, "{}", ch)?)
    }

    fn write_string(&mut self, s: &str, start: usize, end: usize) -> Result<()> {
        for ch in s.chars().take(end).skip(start) {
            self.write_char(ch)?;
        }
        Ok(())
    }

    fn write_u8(&mut self, x: u8) -> Result<()> {
        let n = self.write(&[x])?;
        if n != 1 {
            Err(RuntimeError::WriteError.into())
        } else {
            Ok(())
        }
    }

    fn write_bytevector(&mut self, v: &[u8]) -> Result<()> {
        let n = self.write(v)?;
        if n != 1 {
            Err(RuntimeError::WriteError.into())
        } else {
            Ok(())
        }
    }

    fn flush_output(&mut self) -> Result<()> {
        Ok(self.flush()?)
    }
}

pub fn read_sexpr(port: &mut dyn InputPort) -> Result<Scm> {
    let mut buf = String::new();
    loop {
        match port.read_line()? {
            None => return Ok(Scm::Eof),
            Some(line) => buf += &line,
        }
        let span = Span::new(&buf);
        match parse_sexpr(span) {
            Ok((x, _)) => return Ok((&x).into()),
            Err(e) if e.kind == ParseErrorKind::UnclosedSequence => {}
            Err(e) => return Err(Error::from_parse_error_and_source(e, buf.clone().into())),
        }
    }
}
