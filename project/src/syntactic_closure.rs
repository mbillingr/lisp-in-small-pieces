use crate::env::Env;
use crate::error::Result;
use crate::objectify::Translate;
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::symbol::Symbol;
use crate::syntax::Expression;

#[derive(Debug, Clone)]
pub struct SyntacticClosure {
    sexpr: TrackedSexpr,
    closed_syntactic_environment: Env,
    free_names: Vec<Symbol>,
}

impl PartialEq for SyntacticClosure {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl SyntacticClosure {
    pub fn new(sexpr: TrackedSexpr, env: Env) -> Self {
        SyntacticClosure {
            sexpr,
            closed_syntactic_environment: env,
            free_names: vec![],
        }
    }

    pub fn with_free_names(mut self, names: impl IntoIterator<Item = Symbol>) -> Self {
        self.free_names.extend(names);
        self
    }

    pub fn expand(&self, trans: &mut Translate) -> Result<Expression> {
        let mut combined_env = self.closed_syntactic_environment.clone();
        for name in &self.free_names {
            let var = trans.env.find_variable(name).unwrap();
            combined_env.push_local(var);
        }

        let tmp = std::mem::replace(&mut trans.env, combined_env);
        let result = trans.objectify(&self.sexpr);
        trans.env = tmp;
        result
    }
}

impl From<SyntacticClosure> for TrackedSexpr {
    fn from(sc: SyntacticClosure) -> Self {
        TrackedSexpr {
            src: sc.sexpr.src.clone(),
            sexpr: Sexpr::SyntacticClosure(Box::new(sc)),
        }
    }
}
