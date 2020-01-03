use crate::ast_transform::{Transformer, Visited};
use crate::env::Environment;
use crate::syntax::definition::GlobalDefine;
use crate::syntax::{Definition, Expression, FixLet, Function, GlobalVariable};

pub struct TransformDefines {
    globals: Environment<GlobalVariable>,
    current_context: Context,
}

impl Transformer for TransformDefines {
    fn visit(&mut self, expr: Expression) -> Visited {
        use Expression::*;
        match (expr, &self.current_context) {
            (Definition(x), Context::Global) => self.transform_global_define(x),
            (Definition(x), _) => unimplemented!(),
            (Function(x), _) => self.transform_function(x),
            (x, _) => Visited::Recurse(x),
        }
    }
}

impl TransformDefines {
    pub fn new(globals: Environment<GlobalVariable>) -> Self {
        TransformDefines {
            globals,
            current_context: Context::Global,
        }
    }

    fn transform_global_define(&mut self, node: Definition) -> Visited {
        let form = node.form.transform(self);
        match self.globals.find_variable(&node.var_name) {
            Some(var) => GlobalDefine::new(var, form, node.span).into(),
            None => {
                let var = GlobalVariable::new(node.var_name);
                self.globals.extend(var.clone().into());
                GlobalDefine::new(var, form, node.span).into()
            }
        }
    }

    fn transform_function(&mut self, node: Function) -> Visited {
        let mut scan = ScanOutDefines::new();
        let body = node.body.transform(&mut scan);

        //let body = FixLet::new()
        unimplemented!();

        Function::new(node.variables, body, node.span).into()
    }
}

struct ScanOutDefines {
    defines: Vec<Definition>,
}

impl Transformer for ScanOutDefines {
    fn visit(&mut self, expr: Expression) -> Visited {
        use Expression::*;
        match expr {
            Definition(x) => {
                self.defines.push(x);
                crate::syntax::NoOp.into()
            }
            Sequence(mut x) => {
                *x.first = x.first.transform(self);
                *x.next = x.next.transform(self);
                x.into()
            }
            x => x.into(),
        }
    }
}

impl ScanOutDefines {
    fn new() -> Self {
        ScanOutDefines { defines: vec![] }
    }
}

enum Context {
    Global,
    Body,
    Other,
}
