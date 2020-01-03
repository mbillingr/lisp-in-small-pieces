use crate::env::Env;
use crate::sexpr::TrackedSexpr as Sexpr;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::syntax::{
    Alternative, Expression, FixLet, Function, GlobalAssignment, GlobalReference, GlobalVariable,
    LocalAssignment, LocalReference, LocalVariable, MagicKeyword, PredefinedApplication,
    PredefinedReference, PredefinedVariable, Reference, RegularApplication, Sequence, Variable,
};
use crate::utils::Sourced;
use std::convert::TryInto;

pub type Result<T> = std::result::Result<T, ObjectifyError>;

#[derive(Debug)]
pub struct ObjectifyError {
    pub kind: ObjectifyErrorKind,
    pub location: SourceLocation,
}

#[derive(Debug, PartialEq)]
pub enum ObjectifyErrorKind {
    NoPair,
    IncorrectArity,
    ImmutableAssignment,
    ExpectedList,
    ExpectedSymbol,
}

#[derive(Debug)]
pub struct Translate {
    pub env: Env,
}

impl Translate {
    pub fn new(env: Env) -> Self {
        Translate { env }
    }

    pub fn objectify_toplevel(&mut self, expr: &Sexpr) -> Result<Expression> {
        self.objectify(expr, &self.env.clone())
    }

    pub fn objectify(&mut self, expr: &Sexpr, env: &Env) -> Result<Expression> {
        if expr.is_atom() {
            match () {
                _ if expr.is_symbol() => self.objectify_symbol(expr, env),
                _ => self.objectify_quotation(expr, env),
            }
        } else {
            let m = self.objectify(ocar(expr)?, env)?;
            if let Expression::MagicKeyword(MagicKeyword { name: _, handler }) = m {
                handler(self, expr, env)
            } else {
                self.objectify_application(&m, &ocdr(expr)?, env, expr.source().clone())
            }
        }
    }

    pub fn objectify_quotation(&mut self, expr: &Sexpr, _env: &Env) -> Result<Expression> {
        Ok(Expression::Constant(expr.clone().into()))
    }

    pub fn objectify_alternative(
        &mut self,
        condition: &Sexpr,
        consequence: &Sexpr,
        alternative: Option<&Sexpr>,
        env: &Env,
        span: SourceLocation,
    ) -> Result<Expression> {
        let condition = self.objectify(condition, env)?;
        let consequence = self.objectify(consequence, env)?;
        let alternative = match alternative {
            Some(alt) => self.objectify(alt, env)?,
            None => Expression::Constant(Sexpr::undefined().into()),
        };
        Ok(Alternative::new(condition, consequence, alternative, span).into())
    }

    pub fn objectify_sequence(&mut self, exprs: &Sexpr, env: &Env) -> Result<Expression> {
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

        let mut result = self.objectify(last, env)?;

        for e in sequence.iter().rev() {
            result = Sequence::new(self.objectify(e, env)?, result, exprs.source().clone()).into();
        }

        Ok(result)
    }

    fn objectify_symbol(&mut self, expr: &Sexpr, env: &Env) -> Result<Expression> {
        let var_name = Sexpr::as_symbol(expr).unwrap();
        match env.find_variable(var_name) {
            Some(Variable::LocalVariable(v)) => {
                Ok(LocalReference::new(v, expr.source().clone()).into())
            }
            Some(Variable::GlobalVariable(v)) => {
                Ok(GlobalReference::new(v, expr.source().clone()).into())
            }
            Some(Variable::PredefinedVariable(v)) => {
                Ok(PredefinedReference::new(v, expr.source().clone()).into())
            }
            Some(Variable::MagicKeyword(mkw)) => Ok((mkw).into()),
            Some(Variable::FreeVariable(_)) => {
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
    ) -> Result<Expression> {
        let v = GlobalVariable::new(name);
        env.globals.extend(v.clone().into());
        Ok(GlobalReference::new(v, span).into())
    }

    fn objectify_application(
        &mut self,
        func: &Expression,
        args: &Sexpr,
        env: &Env,
        span: SourceLocation,
    ) -> Result<Expression> {
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

        match func {
            Expression::Function(f) => self.process_closed_application(f.clone(), args, span),
            Expression::Reference(Reference::PredefinedReference(p)) => {
                let fvf = p.var.clone();
                let desc = fvf.description();
                if desc.arity.check(args.len()) {
                    Ok(PredefinedApplication::new(fvf, args, span).into())
                } else {
                    Err(ObjectifyError {
                        kind: ObjectifyErrorKind::IncorrectArity,
                        location: span,
                    })
                }
            }
            _ => Ok(RegularApplication::new(func.clone(), args, span).into()),
        }
    }

    fn process_closed_application(
        &mut self,
        func: Function,
        args: Vec<Expression>,
        span: SourceLocation,
    ) -> Result<Expression> {
        if func
            .variables
            .last()
            .map(LocalVariable::is_dotted)
            .unwrap_or(false)
        {
            self.process_nary_closed_application(func, args, span)
        } else {
            if args.len() == func.variables.len() {
                Ok(FixLet::new(func.variables, args, func.body, span).into())
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
        mut args: Vec<Expression>,
        span: SourceLocation,
    ) -> Result<Expression> {
        let variables = func.variables;
        let body = func.body;

        if args.len() + 1 < variables.len() {
            return Err(ObjectifyError {
                kind: ObjectifyErrorKind::IncorrectArity,
                location: span,
            });
        }

        let cons_var: PredefinedVariable = self
            .env
            .predef
            .find_variable("cons")
            .expect("The cons pritimive must be available in the predefined environment")
            .try_into()
            .unwrap();

        let mut dotted = Expression::Constant(Sexpr::nil(span.last_char()).into());

        while args.len() >= variables.len() {
            let x = args.pop().unwrap();

            let partial_span = span.clone().start_at(x.source());

            dotted =
                PredefinedApplication::new(cons_var.clone(), vec![x, dotted], partial_span).into();
        }

        args.push(dotted.into());

        variables.last().unwrap().set_dotted(false);

        Ok(FixLet::new(variables, args, body, span).into())
    }

    pub fn objectify_function(
        &mut self,
        names: &Sexpr,
        body: &Sexpr,
        env: &Env,
        span: SourceLocation,
    ) -> Result<Expression> {
        let vars = self.objectify_variables_list(names)?;
        env.locals.extend_frame(vars.iter().cloned());
        let bdy = self.objectify_sequence(body, env)?;
        env.locals.pop_frame(vars.len());
        Ok(Function::new(vars, bdy, span).into())
    }

    fn objectify_variables_list(&mut self, names: &Sexpr) -> Result<Vec<LocalVariable>> {
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
                .map(|s| Ok(LocalVariable::new(s?, false, false)))
                .collect::<Result<_>>()?;
            if let Some(x) = dotted {
                let s = x.as_symbol().ok_or_else(|| ObjectifyError {
                    kind: ObjectifyErrorKind::ExpectedSymbol,
                    location: x.source().clone(),
                })?;
                list.push(LocalVariable::new(s.clone(), false, true))
            }
            Ok(list)
        } else if let Some(s) = names.as_symbol() {
            Ok(vec![LocalVariable::new(s.clone(), false, true)])
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
    ) -> Result<Expression> {
        let ov = self.objectify(variable, env)?;
        let of = self.objectify(expr, env)?;

        match ov {
            Expression::Reference(Reference::LocalReference(r)) => {
                r.var.set_mutable(true);
                Ok(LocalAssignment::new(r, of, span).into())
            }
            Expression::Reference(Reference::GlobalReference(r)) => {
                Ok(GlobalAssignment::new(r.var, of, span).into())
            }
            _ => Err(ObjectifyError {
                kind: ObjectifyErrorKind::ImmutableAssignment,
                location: span,
            }),
        }
    }
}

pub fn ocar(e: &Sexpr) -> Result<&Sexpr> {
    e.first().ok_or_else(|| ObjectifyError {
        kind: ObjectifyErrorKind::NoPair,
        location: e.source().clone(),
    })
}

pub fn ocdr(e: &Sexpr) -> Result<Sexpr> {
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
