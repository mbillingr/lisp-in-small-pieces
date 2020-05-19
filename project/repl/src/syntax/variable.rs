use super::keyword::MagicKeyword;
use crate::scm::Scm;
use std::cell::Cell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use sunny_common::Named;
use sunny_common::Symbol;

use sunny_common::{sum_type, sum_types};

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

impl Variable {
    pub fn renamed(&self, newname: Symbol) -> Variable {
        use Variable::*;
        match self {
            LocalVariable(v) => LocalVariable(v.renamed(newname)),
            GlobalVariable(v) => GlobalVariable(v.renamed(newname)),
            MagicKeyword(v) => MagicKeyword(v.renamed(newname)),
            FreeVariable(v) => FreeVariable(v.renamed(newname)),
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

    fn renamed(&self, newname: Symbol) -> LocalVariable {
        LocalVariable::new(newname, self.is_mutable(), self.is_dotted())
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
pub struct GlobalVariable(Rc<(Symbol, Symbol, Cell<VarDef>, Cell<bool>)>);

impl GlobalVariable {
    pub fn new(module: Symbol, name: impl Into<Symbol>, def: VarDef) -> Self {
        let name = name.into();
        GlobalVariable(Rc::new((module, name, Cell::new(def), Cell::new(true))))
    }

    pub fn defined(module: Symbol, name: impl Into<Symbol>, value: Scm) -> Self {
        let name = name.into();
        GlobalVariable(Rc::new((
            module,
            name,
            Cell::new(VarDef::Value(value)),
            Cell::new(true),
        )))
    }

    pub fn constant(module: Symbol, name: impl Into<Symbol>, def: VarDef) -> Self {
        let name = name.into();
        GlobalVariable(Rc::new((module, name, Cell::new(def), Cell::new(false))))
    }

    pub fn module(&self) -> Symbol {
        (&self.0).0
    }

    pub fn value(&self) -> VarDef {
        (&self.0).2.get()
    }

    pub fn set_value(&self, value: VarDef) {
        if !self.is_mutable()
        /*&& self.value() != VarDef::Undefined*/
        {
            eprintln!("WARNING: setting immutable {:?} := {}", self, value)
        }
        (&self.0).2.set(value);
    }

    pub fn ensure_value(&self, value: VarDef) {
        if self.value() != value {
            self.set_value(value)
        }
    }

    pub fn is_mutable(&self) -> bool {
        (&self.0).3.get()
    }

    pub fn set_mutable(&self, d: bool) {
        (&self.0).3.set(d)
    }

    pub fn is_defined(&self) -> bool {
        self.value() != VarDef::Undefined
    }

    fn renamed(&self, newname: Symbol) -> GlobalVariable {
        let gv = GlobalVariable::new(self.module(), newname, self.value());
        gv.set_mutable(self.is_mutable());
        gv
    }

    pub fn full_name(&self) -> Scm {
        Scm::cons(Scm::Symbol(self.module()), Scm::Symbol(self.name()))
    }
}

impl Named for GlobalVariable {
    type Name = Symbol;
    fn name(&self) -> Symbol {
        (&self.0).1
    }
}

impl PartialEq for GlobalVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name().ptr_eq(&other.name())
    }
}

impl Eq for GlobalVariable {}

impl Hash for GlobalVariable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.module().hash(state);
        self.name().hash(state);
    }
}

impl std::fmt::Debug for GlobalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}global {} {}: {}",
            if self.is_mutable() { "" } else { "immutable " },
            self.module(),
            self.name(),
            self.value()
        )
    }
}

#[derive(Clone)]
pub struct FreeVariable(Symbol);

impl FreeVariable {
    pub fn new(name: impl Into<Symbol>) -> Self {
        FreeVariable(name.into())
    }

    fn renamed(&self, newname: Symbol) -> FreeVariable {
        Self::new(newname)
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
            VarDef::Value(x) => write!(f, "{}", x.display()),
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

impl VarDef {
    pub fn as_scm(&self) -> Scm {
        match self {
            VarDef::Unknown => Scm::Undefined,
            VarDef::Undefined => Scm::Uninitialized,
            VarDef::Value(x) => *x,
        }
    }
}
