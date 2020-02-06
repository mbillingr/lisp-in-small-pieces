use super::keyword::MagicKeyword;
use crate::scm::Scm;
use crate::symbol::Symbol;
use crate::utils::Named;
use std::cell::Cell;
use std::rc::Rc;

sum_types! {
    #[derive(Debug, Clone, PartialEq)]
    pub type Variable = LocalVariable
                      | GlobalVariable
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
pub struct GlobalVariable(Rc<Cell<(Symbol, VarDef, bool)>>);

impl GlobalVariable {
    pub fn new(name: impl Into<Symbol>, def: VarDef) -> Self {
        GlobalVariable(Rc::new(Cell::new((name.into(), def, true))))
    }

    pub fn defined(name: impl Into<Symbol>, value: Scm) -> Self {
        GlobalVariable(Rc::new(Cell::new((
            name.into(),
            VarDef::Value(value),
            true,
        ))))
    }

    pub fn constant(name: impl Into<Symbol>, def: VarDef) -> Self {
        GlobalVariable(Rc::new(Cell::new((name.into(), def, false))))
    }

    pub fn value(&self) -> VarDef {
        self.0.get().1
    }

    pub fn set_value(&self, value: VarDef) {
        let (name, old_value, mutable) = self.0.get();
        if mutable || old_value == VarDef::Undefined {
            self.0.set((name, value, true));
        } else {
            panic!("attempt to set immutable {:?} := {}", self, value)
        }
    }

    pub fn ensure_value(&self, value: VarDef) {
        if self.value() != value {
            self.set_value(value)
        }
    }

    pub fn is_mutable(&self) -> bool {
        self.0.get().2
    }
}

impl Named for GlobalVariable {
    type Name = Symbol;
    fn name(&self) -> Symbol {
        self.0.get().0
    }
}

impl PartialEq for GlobalVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name().ptr_eq(&other.name())
    }
}

impl std::fmt::Debug for GlobalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "global {}: {}", self.name(), self.value())
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

#[derive(Debug, Copy, Clone)]
pub enum VarDef {
    Unknown,
    Undefined,
    Value(Scm),
}

impl std::fmt::Display for VarDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VarDef::Unknown => write!(f, "?"),
            VarDef::Undefined => write!(f, "-"),
            VarDef::Value(x) => write!(f, "{}", x),
        }
    }
}

impl PartialEq for VarDef {
    fn eq(&self, other: &Self) -> bool {
        use VarDef::*;
        match (self, other) {
            (Unknown, Unknown) => true,
            (Undefined, Undefined) => true,
            (Value(a), Value(b)) => a.equals(b),
            _ => false,
        }
    }
}
