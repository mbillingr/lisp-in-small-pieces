use super::keyword::MagicKeyword;
use crate::primitive::RuntimePrimitive;
use crate::symbol::Symbol;
use crate::utils::Named;
use std::cell::Cell;
use std::rc::Rc;

sum_types! {
    #[derive(Debug, Clone, PartialEq)]
    pub type Variable = LocalVariable
                      | GlobalVariable
                      | PredefinedVariable
                      | MagicKeyword
                      | FreeVariable;
}

impl Named for Variable {
    type Name = Symbol;
    fn name(&self) -> Self::Name {
        use Variable::*;
        match self {
            LocalVariable(v) => v.name(),
            GlobalVariable(v) => v.name(),
            PredefinedVariable(v) => v.name(),
            MagicKeyword(v) => v.name(),
            FreeVariable(v) => v.name(),
        }
    }
}

#[derive(Clone)]
pub struct LocalVariable(Rc<(Symbol, Cell<bool>, Cell<bool>)>);

impl LocalVariable {
    pub fn new(name: impl Into<Symbol>, mutable: bool, dotted: bool) -> Self {
        LocalVariable(Rc::new((
            name.into(),
            Cell::new(mutable),
            Cell::new(dotted),
        )))
    }

    pub fn is_dotted(&self) -> bool {
        (self.0).2.get()
    }

    pub fn set_dotted(&self, d: bool) {
        (self.0).2.set(d)
    }

    pub fn is_mutable(&self) -> bool {
        (self.0).1.get()
    }

    pub fn set_mutable(&self, d: bool) {
        (self.0).1.set(d)
    }
}

impl Named for LocalVariable {
    type Name = Symbol;
    fn name(&self) -> Symbol {
        (self.0).0
    }
}

impl PartialEq for LocalVariable {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl std::fmt::Debug for LocalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "local{}{} {}",
            if self.is_mutable() { " mut" } else { "" },
            if self.is_dotted() { " dotted" } else { "" },
            self.name()
        )
    }
}

#[derive(Clone)]
pub struct GlobalVariable(Symbol);

impl GlobalVariable {
    pub fn new(name: impl Into<Symbol>) -> Self {
        GlobalVariable(name.into())
    }
}

impl Named for GlobalVariable {
    type Name = Symbol;
    fn name(&self) -> Symbol {
        self.0
    }
}

impl PartialEq for GlobalVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name().ptr_eq(&other.name())
    }
}

impl std::fmt::Debug for GlobalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "global {}", self.name())
    }
}

#[derive(Clone)]
pub struct PredefinedVariable(Symbol, RuntimePrimitive);

impl PredefinedVariable {
    pub fn new(name: impl Into<Symbol>, func: RuntimePrimitive) -> Self {
        PredefinedVariable(name.into(), func)
    }

    pub fn procedure(&self) -> &RuntimePrimitive {
        &self.1
    }
}

impl Named for PredefinedVariable {
    type Name = Symbol;
    fn name(&self) -> Symbol {
        self.0
    }
}

impl PartialEq for PredefinedVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name().ptr_eq(&other.name())
    }
}

impl std::fmt::Debug for PredefinedVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "predefined {}", self.name())
    }
}

#[derive(Clone)]
pub struct FreeVariable(Symbol);

impl FreeVariable {
    pub fn new(name: impl Into<Symbol>) -> Self {
        FreeVariable(name.into())
    }
}

impl Named for FreeVariable {
    type Name = Symbol;
    fn name(&self) -> Symbol {
        self.0
    }
}

impl PartialEq for FreeVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name().ptr_eq(&other.name())
    }
}

impl std::fmt::Debug for FreeVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "free {}", self.name())
    }
}
