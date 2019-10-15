use crate::environments::init_predefined;
use crate::error::{Error, Result};
use crate::opcode::Op;
use crate::types::{ActivationFrame, CodePointer, OpaqueCast, Scm};
use crate::vm::VirtualMachine;
use lisp_core::lexpr;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct SchemeObjectFile {
    pub dynamic_vars: Vec<String>,
    pub global_vars: Vec<String>,
    pub constants: Vec<Scm>,
    pub bytecode: &'static [u8],
    pub entry_points: Vec<usize>,
}

impl SchemeObjectFile {
    pub fn from_file(filename: &str) -> Result<Self> {
        let mut data = String::new();
        File::open(filename)?.read_to_string(&mut data)?;
        data = format!("({})", data);

        let value = lexpr::from_str(&data)?;

        let dynamic_vars = list_of_symbols(&value[0])?;
        let global_vars = list_of_symbols(&value[1])?;
        let constants = vec_of_values(&value[2])?;
        let bytecode = vec_of_bytes(&value[3])?;
        let entry_points = entries(&value[4])?;

        Ok(SchemeObjectFile {
            dynamic_vars,
            global_vars,
            constants,
            bytecode: Box::leak(bytecode.into_boxed_slice()),
            entry_points,
        })
    }

    pub fn into_vm(self) -> VirtualMachine {
        let mut init_stack = unsafe { vec![CodePointer::new_unchecked(&self.bytecode[1]).as_op()] }; // pc for FINISH
        for e in self.entry_points.into_iter().rev() {
            unsafe {
                init_stack.push(CodePointer::new_unchecked(&self.bytecode[e]).as_op());
            }
        }
        VirtualMachine {
            val: Scm::uninitialized(),
            fun: Scm::uninitialized(),
            arg1: Scm::uninitialized(),
            arg2: Scm::uninitialized(),
            env: ActivationFrame::allocate(0),
            constants: self.constants,
            mut_globals: vec![Scm::uninitialized(); self.global_vars.len()],
            globals: init_predefined(),
            stack: Vec::with_capacity(1024),
            init_stack,
            pc: CodePointer::new(&Op::Finish),
            statistics: vec![Default::default(); 256],
            max_stack: 0,
        }
    }
}

fn list_of_symbols(mut val: &lexpr::Value) -> Result<Vec<String>> {
    let mut list = vec![];
    while let Some((car, cdr)) = val.as_pair() {
        match car.as_symbol() {
            Some(s) => list.push(s.to_string()),
            None => return Err(Error::UnexpectedType(car.clone())),
        }
        val = cdr;
    }
    Ok(list)
}

fn vec_of_values(val: &lexpr::Value) -> Result<Vec<Scm>> {
    match val {
        lexpr::Value::Vector(data) => Ok(data.iter().map(Into::into).collect()),
        _ => Err(Error::UnexpectedType(val.clone())),
    }
}

fn vec_of_bytes(val: &lexpr::Value) -> Result<Vec<u8>> {
    match val {
        lexpr::Value::Vector(data) => Ok(data.iter().map(|x| x.as_u64().unwrap() as u8).collect()),
        _ => Err(Error::UnexpectedType(val.clone())),
    }
}

fn entries(mut val: &lexpr::Value) -> Result<Vec<usize>> {
    let mut list = vec![];
    while let Some((car, cdr)) = val.as_pair() {
        match car.as_u64() {
            Some(u) => list.push(u as usize),
            None => return Err(Error::UnexpectedType(car.clone())),
        }
        val = cdr;
    }
    match val.as_u64() {
        Some(u) => list.push(u as usize),
        None if list.is_empty() => return Err(Error::UnexpectedType(val.clone())),
        None => {}
    }
    Ok(list)
}
