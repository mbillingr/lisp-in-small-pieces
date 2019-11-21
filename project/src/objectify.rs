use crate::ast::{AstNode, Constant, FixLet, Function, GlobalReference, LocalReference, MagicKeyword, PredefinedApplication, PredefinedReference, RegularApplication, Sequence, Variable, LocalAssignment, GlobalAssignment};
use crate::env::{Env, EnvAccess};
use crate::sexpr::Sexpr;
use crate::value::{Symbol, Value};

pub type Result<T> = std::result::Result<T, ObjectifyError>;

#[derive(Debug)]
pub enum ObjectifyError {
    NoPair,
    IncorrectArity,
    ImmutableAssignment,
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
                self.objectify_application(m, cdr(expr)?, env)
            }
        }
    }

    fn objectify_quotation(&mut self, expr: &Sexpr, _env: &Env) -> Result<AstNode> {
        Ok(Constant::new(expr.clone()))
    }

    pub fn objectify_sequence(&mut self, exprs: &Sexpr, env: &Env) -> Result<AstNode> {
        if let Some((first, rest)) = exprs.as_pair() {
            let a = self.objectify(first, env)?;
            if rest.is_pair() {
                Ok(Sequence::new(a, self.objectify_sequence(rest, env)?))
            } else {
                Ok(a)
            }
        } else {
            unimplemented!("empty sequence")
        }
    }

    fn objectify_symbol(&mut self, expr: &Sexpr, env: &Env) -> Result<AstNode> {
        let var_name = Sexpr::as_symbol(expr).unwrap();
        match env.find_variable(&var_name) {
            Some(v @ Variable::Local(_, _, _)) => Ok(LocalReference::new(v)),
            Some(v @ Variable::Global(_)) => Ok(GlobalReference::new(v)),
            Some(v @ Variable::Predefined(_, _)) => Ok(PredefinedReference::new(v)),
            Some(Variable::Macro(mkw)) => Ok(mkw),
            None => self.objectify_free_reference(var_name, env),
        }
    }

    fn objectify_free_reference(&mut self, name: Symbol, env: &Env) -> Result<AstNode> {
        let v = Variable::Global(name);
        env.insert_global(v.clone());
        Ok(GlobalReference::new(v))
    }

    fn objectify_application(
        &mut self,
        mut func: AstNode,
        args: &Value,
        env: &Env,
    ) -> Result<AstNode> {
        let args = map(args, |e| self.objectify(e, env))?;

        match func.downcast::<Function>() {
            Ok(f) => return self.process_closed_application(*f, args),
            Err(f) => func = f,
        }

        if let Some(p) = func.downcast_ref::<PredefinedReference>() {
            let fvf = p.variable().clone();
            let desc = fvf.description();
            if desc.arity.check(args.len()) {
                Ok(PredefinedApplication::new(fvf, args))
            } else {
                Err(ObjectifyError::IncorrectArity)
            }
        } else {
            Ok(RegularApplication::new(func, args))
        }
    }

    /*fn convert2arguments<I>(args: impl IntoIterator<Item = AstNode, IntoIter = I>) -> AstNode
    where
        I: DoubleEndedIterator<Item = AstNode>,
    {
        let mut result: AstNode = NoMoreArguments::new();
        for a in args.into_iter().rev() {
            result = Arguments::new(a, result);
        }
        result
    }*/

    fn process_closed_application(
        &mut self,
        func: Function,
        args: Vec<AstNode>,
    ) -> Result<AstNode> {
        if func
            .variables
            .last()
            .map(Variable::is_dotted)
            .unwrap_or(false)
        {
            self.process_nary_closed_application(func, args)
        } else {
            if args.len() == func.variables.len() {
                Ok(FixLet::new(func.variables, args, func.body))
            } else {
                Err(ObjectifyError::IncorrectArity)
            }
        }
    }

    fn process_nary_closed_application(
        &mut self,
        func: Function,
        mut args: Vec<AstNode>,
    ) -> Result<AstNode> {
        let variables = func.variables;
        let body = func.body;

        if args.len() + 1 < variables.len() {
            return Err(ObjectifyError::IncorrectArity);
        }

        let cons_var = self
            .predef_env
            .find_variable("cons")
            .expect("The cons pritimive must be available in the predefined environment");

        let mut dotted: AstNode = Constant::new(Value::nil());

        while args.len() >= variables.len() {
            let x = args.pop().unwrap();

            // todo: generate list construction by chaining applications of predefined 'cons
            dotted = PredefinedApplication::new(cons_var.clone(), vec![x, dotted]);
        }

        args.push(dotted);

        variables.last().unwrap().set_dotted(false);

        Ok(FixLet::new(variables, args, body))
    }

    pub fn objectify_function(
        &mut self,
        names: &Sexpr,
        body: &Sexpr,
        env: &Env,
    ) -> Result<AstNode> {
        let vars = self.objectify_variables_list(names);
        let bdy = self.objectify_sequence(body, &env.clone().extend_frame(vars.iter().cloned()))?;
        Ok(Function::new(vars, bdy))
    }

    fn objectify_variables_list(&mut self, mut names: &Sexpr) -> Vec<Variable> {
        let mut list = vec![];
        while let Some((car, cdr)) = names.as_pair() {
            list.push(Variable::local(
                Sexpr::as_symbol(car).expect("Not a symbol in variables list"),
                false,
                false,
            ));
            names = cdr;
        }
        if let Some(name) = Sexpr::as_symbol(names) {
            list.push(Variable::local(name, false, true));
        }
        list
    }

    pub fn objectify_assignment(&mut self, variable: &Sexpr, expr: &Sexpr, env: &Env) -> Result<AstNode> {
        let ov = self.objectify(variable, env)?;
        let of = self.objectify(expr, env)?;

        let ov = match ov.downcast::<LocalReference>() {
            Ok(r) => {
                r.variable().set_mutable(true);
                return Ok(LocalAssignment::new(*r, of))
            }
            Err(ov) => ov,
        };

        let ov = match ov.downcast::<GlobalReference>() {
            Ok(r) => {
                return Ok(GlobalAssignment::new(r.variable().clone(), of))
            }
            Err(ov) => ov,
        };

        Err(ObjectifyError::ImmutableAssignment)
    }
}

pub fn map<T>(mut sequence: &Value, mut f: impl FnMut(&Value) -> Result<T>) -> Result<Vec<T>> {
    let mut result = vec![];
    while let Some((car, cdr)) = sequence.as_pair() {
        result.push(f(car)?);
        sequence = cdr;
    }
    Ok(result)
}

pub fn car(e: &Sexpr) -> Result<&Sexpr> {
    //e.car().ok_or(ObjectifyError::NoPair)
}

pub fn cdr(e: &Sexpr) -> Result<&Sexpr> {
    //e.cdr().ok_or(ObjectifyError::NoPair)
}
