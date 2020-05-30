use super::keyword::MagicKeyword;
use crate::scm::Scm;
use std::cell::{Cell, RefCell};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use sunny_common::{sum_type, sum_types, Named, Symbol};

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
pub struct GlobalObject(Rc<String>);

impl GlobalObject {
    pub fn new(module: &str, name: &str) -> Self {
        GlobalObject(Rc::new(format!("{}/{}", module, name)))
    }

    pub fn id(&self) -> &str {
        &*self.0
    }
}

impl std::fmt::Debug for GlobalObject {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone)]
pub struct GlobalVariable(Rc<(Symbol, Symbol, RefCell<Option<GlobalObject>>, Cell<bool>)>);

impl GlobalVariable {
    pub fn defined(module: Symbol, name: impl Into<Symbol>, obj: GlobalObject) -> Self {
        let name = name.into();
        GlobalVariable(Rc::new((
            module,
            name,
            RefCell::new(Some(obj)),
            Cell::new(true),
        )))
    }

    pub fn undefined(module: Symbol, name: impl Into<Symbol>) -> Self {
        let name = name.into();
        GlobalVariable(Rc::new((module, name, RefCell::new(None), Cell::new(true))))
    }

    pub fn constant(module: Symbol, name: impl Into<Symbol>, obj: GlobalObject) -> Self {
        let name = name.into();
        GlobalVariable(Rc::new((
            module,
            name,
            RefCell::new(Some(obj)),
            Cell::new(false),
        )))
    }

    pub fn module(&self) -> Symbol {
        (&self.0).0
    }

    pub fn is_mutable(&self) -> bool {
        (&self.0).3.get()
    }

    pub fn set_mutable(&self, d: bool) {
        (&self.0).3.set(d)
    }

    pub fn object(&self) -> Option<GlobalObject> {
        (&self.0).2.borrow().clone()
    }

    pub fn is_defined(&self) -> bool {
        self.object().is_some()
    }

    pub fn set_defined(&self, obj: GlobalObject) {
        *(&self.0).2.borrow_mut() = Some(obj)
    }

    fn renamed(&self, newname: Symbol) -> GlobalVariable {
        let gv = GlobalVariable::undefined(self.module(), newname);
        if let Some(obj) = self.object() {
            *(&gv.0).2.borrow_mut() = Some(obj.clone());
        }
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
            "{}global {} {}",
            if self.is_mutable() { "" } else { "immutable " },
            self.module(),
            self.name()
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
