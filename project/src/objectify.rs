use crate::ast::{
    Alternative, AstNode, Constant, FixLet, Function, GlobalAssignment, GlobalReference,
    LocalAssignment, LocalReference, MagicKeyword, PredefinedApplication, PredefinedReference, Ref,
    RegularApplication, Sequence, Variable,
};
use crate::env::{Env, EnvAccess};
use crate::sexpr::TrackedSexpr as Sexpr;
use crate::source::SourceLocation;
use crate::symbol::Symbol;

pub type Result<T> = std::result::Result<T, ObjectifyError>;

#[derive(Debug)]
pub struct ObjectifyError {
    pub kind: ObjectifyErrorKind,
    pub location: SourceLocation,
}

#[derive(Debug)]
pub enum ObjectifyErrorKind {
    NoPair,
    IncorrectArity,
    ImmutableAssignment,
    ExpectedList,
    ExpectedSymbol,
}

pub struct Translate {
    pub predef_env: Env,
    pub global_env: Env,
}

impl Translate {
    pub fn from_predefined(predef_env: Env) -> Self {
        let global_env = predef_env.clone().mark_global();
        Translate {
            predef_env,
            global_env,
        }
    }

    pub fn objectify_toplevel(&mut self, expr: &Sexpr) -> Result<AstNode> {
        let env = self.global_env.clone();
        self.objectify(expr, &env)
    }

    pub fn objectify(&mut self, expr: &Sexpr, env: &Env) -> Result<AstNode> {
        if expr.is_atom() {
            match () {
                _ if expr.is_symbol() => self.objectify_symbol(expr, env),
                _ => self.objectify_quotation(expr, env),
            }
        } else {
            let m = self.objectify(car(expr)?, env)?;
            if let Some(MagicKeyword { name: _, handler }) = m.downcast_ref() {
                handler(self, expr, env)
            } else {
                self.objectify_application(m, &cdr(expr)?, env, expr.source().clone())
            }
        }
    }

    pub fn objectify_quotation(&mut self, expr: &Sexpr, _env: &Env) -> Result<AstNode> {
        Ok(Constant::new(expr.clone()))
    }

    pub fn objectify_alternative(
        &mut self,
        condition: &Sexpr,
        consequence: &Sexpr,
        alternative: &Sexpr,
        env: &Env,
        span: SourceLocation,
    ) -> Result<AstNode> {
        let condition = self.objectify(condition, env)?;
        let consequence = self.objectify(consequence, env)?;
        let alternative = self.objectify(alternative, env)?;
        Ok(Alternative::new(condition, consequence, alternative, span))
    }

    pub fn objectify_sequence(&mut self, exprs: &Sexpr, env: &Env) -> Result<AstNode> {
        let mut sequence = exprs
            .as_proper_list()
            .ok_or_else(|| ObjectifyError {
                kind: ObjectifyErrorKind::ExpectedList,
                location: exprs.source().clone(),
            })?
            .clone();

        let last = if let Some(x) = sequence.pop() {
            x
        } else {
            unimplemented!("empty sequence")
        };

        let mut result: AstNode = self.objectify(last, env)?;

        for e in sequence.iter().rev() {
            result = Sequence::new(self.objectify(e, env)?, result, exprs.source().clone());
        }

        Ok(result)
    }

    fn objectify_symbol(&mut self, expr: &Sexpr, env: &Env) -> Result<AstNode> {
        let var_name = Sexpr::as_symbol(expr).unwrap();
        match env.find_variable(var_name) {
            Some(v @ Variable::Local(_)) => Ok(LocalReference::new(v, expr.source().clone())),
            Some(v @ Variable::Global(_)) => Ok(GlobalReference::new(v, expr.source().clone())),
            Some(v @ Variable::Predefined(_)) => {
                Ok(PredefinedReference::new(v, expr.source().clone()))
            }
            Some(Variable::Macro(mkw)) => Ok(Ref::new((*mkw).clone())),
            Some(Variable::Free(_)) => {
                panic!("There should be no free variables in the compile-time environment")
            }
            None => self.objectify_free_reference(var_name.clone(), env, expr.source().clone()),
        }
    }

    fn objectify_free_reference(
        &mut self,
        name: Symbol,
        env: &Env,
        span: SourceLocation,
    ) -> Result<AstNode> {
        let v = Variable::Global(name);
        env.insert_global(v.clone());
        Ok(GlobalReference::new(v, span))
    }

    fn objectify_application(
        &mut self,
        func: AstNode,
        args: &Sexpr,
        env: &Env,
        span: SourceLocation,
    ) -> Result<AstNode> {
        let args = if args.is_null() {
            vec![]
        } else {
            args.as_proper_list()
                .ok_or_else(|| ObjectifyError {
                    kind: ObjectifyErrorKind::ExpectedList,
                    location: args.source().clone(),
                })?
                .iter()
                .map(|e| self.objectify(e, env))
                .collect::<Result<_>>()?
        };

        if let Some(f) = func.downcast_ref::<Function>() {
            return self.process_closed_application(f.clone(), args, span);
        }

        if let Some(p) = func.downcast_ref::<PredefinedReference>() {
            let fvf = p.variable().clone();
            let desc = fvf.description();
            if desc.arity.check(args.len()) {
                Ok(PredefinedApplication::new(fvf, args, span))
            } else {
                Err(ObjectifyError {
                    kind: ObjectifyErrorKind::IncorrectArity,
                    location: span,
                })
            }
        } else {
            Ok(RegularApplication::new(func, args, span))
        }
    }

    fn process_closed_application(
        &mut self,
        func: Function,
        args: Vec<AstNode>,
        span: SourceLocation,
    ) -> Result<AstNode> {
        if func
            .variables
            .last()
            .map(Variable::is_dotted)
            .unwrap_or(false)
        {
            self.process_nary_closed_application(func, args, span)
        } else {
            if args.len() == func.variables.len() {
                Ok(FixLet::new(func.variables, args, func.body, span))
            } else {
                Err(ObjectifyError {
                    kind: ObjectifyErrorKind::IncorrectArity,
                    location: span,
                })
            }
        }
    }

    fn process_nary_closed_application(
        &mut self,
        func: Function,
        mut args: Vec<AstNode>,
        span: SourceLocation,
    ) -> Result<AstNode> {
        let variables = func.variables;
        let body = func.body;

        if args.len() + 1 < variables.len() {
            return Err(ObjectifyError {
                kind: ObjectifyErrorKind::IncorrectArity,
                location: span,
            });
        }

        let cons_var = self
            .predef_env
            .find_variable("cons")
            .expect("The cons pritimive must be available in the predefined environment");

        let mut dotted: AstNode = Constant::new(Sexpr::nil(span.last_char()));

        while args.len() >= variables.len() {
            let x = args.pop().unwrap();

            let partial_span = span.clone().start_at(x.source());

            // todo: generate list construction by chaining applications of predefined 'cons
            dotted = PredefinedApplication::new(cons_var.clone(), vec![x, dotted], partial_span);
        }

        args.push(dotted);

        variables.last().unwrap().set_dotted(false);

        Ok(FixLet::new(variables, args, body, span))
    }

    pub fn objectify_function(
        &mut self,
        names: &Sexpr,
        body: &Sexpr,
        env: &Env,
        span: SourceLocation,
    ) -> Result<AstNode> {
        println!("{:?}", names);
        let vars = self.objectify_variables_list(names)?;
        let bdy = self.objectify_sequence(body, &env.clone().extend_frame(vars.iter().cloned()))?;
        Ok(Function::new(vars, bdy, span))
    }

    fn objectify_variables_list(&mut self, names: &Sexpr) -> Result<Vec<Variable>> {
        if let Some((l, dotted)) = names.as_list() {
            let mut list: Vec<_> = (**l)
                .iter()
                .map(|x| {
                    x.as_symbol().ok_or_else(|| ObjectifyError {
                        kind: ObjectifyErrorKind::ExpectedSymbol,
                        location: x.source().clone(),
                    })
                })
                .map(|s| Ok(s?.clone()))
                .map(|s| Ok(Variable::local(s?, false, false)))
                .collect::<Result<_>>()?;
            if let Some(x) = dotted {
                let s = x.as_symbol().ok_or_else(|| ObjectifyError {
                    kind: ObjectifyErrorKind::ExpectedSymbol,
                    location: x.source().clone(),
                })?;
                list.push(Variable::local(s.clone(), false, true))
            }
            Ok(list)
        } else if let Some(s) = names.as_symbol() {
            Ok(vec![Variable::local(s.clone(), false, true)])
        } else if names.is_null() {
            Ok(vec![])
        } else {
            Err(ObjectifyError {
                kind: ObjectifyErrorKind::ExpectedList,
                location: names.source().clone(),
            })
        }
    }

    pub fn objectify_assignment(
        &mut self,
        variable: &Sexpr,
        expr: &Sexpr,
        env: &Env,
        span: SourceLocation,
    ) -> Result<AstNode> {
        let ov = self.objectify(variable, env)?;
        let of = self.objectify(expr, env)?;

        if let Some(r) = ov.downcast_ref::<LocalReference>() {
            r.variable().set_mutable(true);
            Ok(LocalAssignment::new(r.clone(), of, span))
        } else if let Some(r) = ov.downcast_ref::<GlobalReference>() {
            Ok(GlobalAssignment::new(r.variable().clone(), of, span))
        } else {
            Err(ObjectifyError {
                kind: ObjectifyErrorKind::ImmutableAssignment,
                location: span,
            })
        }
    }
}

pub fn car(e: &Sexpr) -> Result<&Sexpr> {
    e.first().ok_or_else(|| ObjectifyError {
        kind: ObjectifyErrorKind::NoPair,
        location: e.source().clone(),
    })
}

pub fn cdr(e: &Sexpr) -> Result<Sexpr> {
    e.tail().ok_or_else(|| ObjectifyError {
        kind: ObjectifyErrorKind::NoPair,
        location: e.source().clone(),
    })
}

pub fn decons(e: &Sexpr) -> Result<(&Sexpr, Sexpr)> {
    e.decons().ok_or_else(|| ObjectifyError {
        kind: ObjectifyErrorKind::NoPair,
        location: e.source().clone(),
    })
}
