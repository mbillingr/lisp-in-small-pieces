use crate::error::{Error, Result};
use crate::scm::{Result as ScmResult, Scm};
use crate::sexpr::TrackedSexpr;
use crate::syntax::variable::GlobalObject;
use crate::syntax::MagicKeyword;
use std::collections::HashMap;
use std::path::PathBuf;
use sunny_common::Symbol;

pub type CompileLibrary = HashMap<Symbol, ExportItem>;
pub type RuntimeLibrary = HashMap<Symbol, Scm>;

#[derive(Debug, Clone)]
pub struct LibraryData {
    pub exports: CompileLibrary,
    pub values: RuntimeLibrary,
}

#[derive(Debug, Clone)]
pub enum ExportItem {
    Value(GlobalObject),
    Macro(MagicKeyword),
}

impl LibraryData {
    pub fn new() -> Self {
        LibraryData {
            exports: HashMap::new(),
            values: HashMap::new(),
        }
    }

    /*pub fn lookup(&self, identifier: Symbol) -> Option<&ExportItem> {
        self.exports.get(&identifier)
    }

    pub fn all_exports(&self) -> impl Iterator<Item = (Symbol, &ExportItem)> {
        self.exports.iter().map(|(s, item)| (*s, item))
    }*/
}

pub struct LibraryBuilder {
    lib: LibraryData,
    name: Symbol,
}

impl LibraryBuilder {
    pub fn new(name: impl Into<Symbol>) -> Self {
        LibraryBuilder {
            lib: LibraryData::new(),
            name: name.into(),
        }
    }

    pub fn set_name(self, name: impl Into<Symbol>) -> Self {
        LibraryBuilder {
            name: name.into(),
            ..self
        }
    }

    pub fn build(self) -> LibraryData {
        self.lib
    }

    /*pub fn add_identifier(mut self, identifier: impl Into<Symbol>) -> Self {
        self.lib
            .exports
            .insert(identifier.into(), ExportItem::Value(VarDef::Unknown));
        self
    }*/

    pub fn add_value(mut self, identifier: impl Into<Symbol>, value: impl Into<Scm>) -> Self {
        let name = identifier.into();
        let value = value.into();
        self.lib.exports.insert(
            name,
            ExportItem::Value(GlobalObject::new(self.name.as_str(), name.as_str())),
        );
        self.lib.values.insert(name, value);
        self
    }

    pub fn add_macro(mut self, identifier: impl Into<Symbol>, the_macro: MagicKeyword) -> Self {
        self.lib
            .exports
            .insert(identifier.into(), ExportItem::Macro(the_macro));
        self
    }
}

pub fn is_import(expr: &TrackedSexpr) -> bool {
    expr.car().map(|car| car == "import").unwrap_or(false)
}

pub fn libname_to_path(mut expr: &TrackedSexpr) -> Result<PathBuf> {
    let mut path = PathBuf::new();

    while expr.is_pair() {
        path.push(format!("{}", expr.car()?));
        expr = expr.cdr()?;
    }

    Ok(path)
}

/// Build library definition.
///
/// - Macros are compile-time expanders and have no runtime representation
/// - Primitives are functions that take a slice of arguments and the current context
/// - Natives are Rust functions that take each argument separately (they get wrapped in a primitive)
#[macro_export]
macro_rules! build_library {
    () => {build_library!(@ |x|x;)};

    (macro $($rest:tt)*) => {
        build_library!(@|builder| builder; macro $($rest)*)
    };

    (primitive $($rest:tt)*) => {
        build_library!(@|builder| builder; primitive $($rest)*)
    };

    (native $($rest:tt)*) => {
        build_library!(@|builder| builder; native $($rest)*)
    };

    ($name:expr; $($rest:tt)*) => {
        build_library!(@|builder| builder; name $name; $($rest)*)
    };

    (@$build:expr; macro $name:expr, $func:expr; $($rest:tt)*) => {
        build_library!(
            @|builder: LibraryBuilder| $build(builder).add_macro($name, MagicKeyword::new($name, $func));
            $($rest)*)
    };

    (@$build:expr; primitive $name:expr, $arity_test:tt $arity:tt, $func:expr; $($rest:tt)*) => {
        build_library!(
            @|builder: LibraryBuilder| $build(builder).add_value(
                $name,
                Scm::Primitive(make_primitive!($name, $arity_test$arity, $func)));
            $($rest)*)
    };

    (@$build:expr; native $name:expr, $arity_test:tt $arity:tt, $func:expr; $($rest:tt)*) => {
        build_library!(
            @|builder: LibraryBuilder| $build(builder).add_value(
                $name,
                Scm::Primitive(make_primitive!(native $name, $arity_test$arity, $func)));
            $($rest)*)
    };

    (@$build:expr; name $name:expr; $($rest:tt)*) => {
        build_library!(
            @|builder: LibraryBuilder| $build(builder).set_name($name);
            $($rest)*)
    };

    (@$build:expr;) => {
        $build(LibraryBuilder::new("")).build()
    };
}

macro_rules! make_primitive {
    (native $name:expr, =$arity:tt, $func:expr) => {
        make_primitive!(
            $name,
            =$arity,
             wrap_native!(=$arity $func))
    };

    (native $name:expr, >=$arity:tt, $func:expr) => {
        make_primitive!(
            $name,
            >=$arity,
             wrap_native!(>=$arity $func))
    };

    ($name:expr, =$arity:expr, $func:expr) => {
        RuntimePrimitive::new(
            $name,
            Arity::Exact($arity),
            |args, ctx| $func(args, ctx).wrap())
    };

    ($name:expr, >=$arity:expr, $func:expr) => {
        RuntimePrimitive::new(
            $name,
            Arity::AtLeast($arity),
            |args, ctx| $func(args, ctx).wrap())
    };
}

macro_rules! wrap_native {
    (=$arity:tt $func:expr) => {
        |args: &[Scm], _ctx: &VirtualMachine| -> Result<Scm> { wrap_native!(@inner args =$arity $func) }
    };

    (>=$arity:tt $func:expr) => {
        |args: &[Scm], _ctx: &VirtualMachine| -> Result<Scm> { wrap_native!(@inner args >=$arity $func) }
    };

    (@inner $args:ident =0 $func:expr) => {
        match &$args[..] {
            [] => $func().wrap(),
            _ => unreachable!(),
        }
    };

    (@inner $args:ident =1 $func:expr) => {
        match &$args[..] {
            [a] => $func(a.try_into()?).wrap(),
            _ => unreachable!(),
        }
    };

    (@inner $args:ident =2 $func:expr) => {
        match &$args[..] {
            [a, b] => $func(a.try_into()?, b.try_into()?).wrap(),
            _ => unreachable!(),
        }
    };

    (@inner $args:ident =3 $func:expr) => {
        match &$args[..] {
            [a, b, c] => $func(a.try_into()?, b.try_into()?, c.try_into()?).wrap(),
            _ => unreachable!(),
        }
    };

    (@inner $args:ident =4 $func:expr) => {
        match &$args[..] {
            [a, b, c, d] => $func(a.try_into()?, b.try_into()?, c.try_into()?, d.try_into()?).wrap(),
            _ => unreachable!(),
        }
    };

    (@inner $args:ident =5 $func:expr) => {
        match &$args[..] {
            [a, b, c, d, e] => $func(a.try_into()?, b.try_into()?, c.try_into()?, d.try_into()?, e.try_into()?).wrap(),
            _ => unreachable!(),
        }
    };

    (@inner $args:ident >=0 $func:expr) => {
        $func($args).wrap()
    };

    (@inner $args:ident >=1 $func:expr) => {
        $func($args[0].try_into()?, &$args[1..]).wrap()
    };

    (@inner $args:ident >=2 $func:expr) => {
        $func($args[0].try_into()?, $args[1].try_into()?, &$args[2..]).wrap()
    };

    (@inner $args:ident >=3 $func:expr) => {
        $func($args[0].try_into()?, $args[1].try_into()?, $args[2].try_into()?, &$args[3..]).wrap()
    };
}

pub trait ResultWrap {
    fn wrap(self) -> Result<Scm>;
}

impl<T> ResultWrap for T
where
    T: Into<Scm>,
{
    fn wrap(self) -> Result<Scm> {
        Ok(self.into())
    }
}

impl<T> ResultWrap for Result<T>
where
    T: Into<Scm>,
{
    fn wrap(self) -> Result<Scm> {
        self.map(T::into)
    }
}

impl<T> ResultWrap for ScmResult<T>
where
    T: Into<Scm>,
{
    fn wrap(self) -> Result<Scm> {
        self.map(T::into).map_err(Error::from)
    }
}

impl ResultWrap for () {
    fn wrap(self) -> Result<Scm> {
        Ok(Scm::Undefined)
    }
}

impl ResultWrap for Result<()> {
    fn wrap(self) -> Result<Scm> {
        self.map(|_| Scm::Undefined)
    }
}

impl ResultWrap for ScmResult<()> {
    fn wrap(self) -> Result<Scm> {
        self.map(|_| Scm::Undefined).map_err(Error::from)
    }
}
