use crate::scm::Scm;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    ValueStackUnderflow,
}

#[derive(Debug)]
pub struct CodeObject {
    constants: Box<[Scm]>,
    code: Box<[Op]>,
}

#[derive(Debug)]
pub enum Op {
    Constant(usize),
    JumpFalse(isize),
    Jump(isize),
    Return,

    Drop,
}

impl CodeObject {
    pub fn new(code: impl Into<Box<[Op]>>, constants: impl Into<Box<[Scm]>>) -> Self {
        CodeObject {
            constants: constants.into(),
            code: code.into(),
        }
    }
}

pub fn eval_code_object(code: &'static CodeObject) -> Result<Scm> {
    let mut ip: isize = 0;

    let mut value_stack = vec![];

    loop {
        let op = &code.code[ip as usize];
        ip += 1;
        match *op {
            Op::Constant(idx) => value_stack.push(code.constants[idx]),
            Op::Jump(delta) => ip += delta,
            Op::JumpFalse(delta) => {
                if value_stack
                    .pop()
                    .ok_or(Error::ValueStackUnderflow)?
                    .is_false()
                {
                    ip += delta
                }
            }
            Op::Return => return value_stack.pop().ok_or(Error::ValueStackUnderflow), // TODO: return to previous function
            Op::Drop => {
                value_stack.pop().ok_or(Error::ValueStackUnderflow)?;
            }
        }
    }
}
