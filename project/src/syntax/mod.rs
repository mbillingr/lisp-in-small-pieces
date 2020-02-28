pub mod alternative;
pub mod application;
pub mod assignment;
pub mod boxes;
pub mod closure;
pub mod constant;
pub mod definition;
pub mod expression;
pub mod fixlet;
pub mod function;
pub mod import;
pub mod keyword;
pub mod let_continuation;
pub mod library;
pub mod noop;
pub mod program;
pub mod reference;
pub mod sequence;
pub mod variable;

pub use alternative::Alternative;
pub use application::Application;
pub use assignment::{Assignment, GlobalAssignment, LocalAssignment};
pub use boxes::{BoxCreate, BoxRead, BoxWrite};
pub use closure::FlatClosure;
pub use constant::Constant;
pub use expression::Expression;
pub use fixlet::FixLet;
pub use function::Function;
pub use import::{Import, ImportItem, ImportSet};
pub use keyword::{MagicKeyword, MagicKeywordHandler};
pub use let_continuation::{LetContKind, LetContinuation};
pub use library::{Library, LibraryDeclaration, LibraryExport};
pub use noop::NoOp;
pub use program::Program;
pub use reference::{FreeReference, GlobalReference, LocalReference, Reference};
pub use sequence::Sequence;
pub use variable::{FreeVariable, GlobalVariable, LocalVariable, Variable};

use crate::scm::Scm;

pub trait Reify {
    fn reify(&self) -> Scm;
}
