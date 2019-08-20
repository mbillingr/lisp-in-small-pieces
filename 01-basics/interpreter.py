import sys
sys.path.append("../libs")
from utils import read, is_null, is_symbol, is_pair, cons, length, Pair, Symbol, Nil

# This Python implementation uses Lisp idioms rather than Python idioms.
# In particular, it uses "unpythonic" recursion to loop over linked lists,
# which makes the code vary similar to the original Scheme implementation.
# Unfortunately, Python does not have tail call optimization, thus this
# interpreter will likely overflow the call stack for complexer programs.
#
# Still, it is educational to see that it is possible to implement lambdas
# using lambdas/closures from the host function, even though Python is a
# different language that Lisp.

def main():
    definitial('x')
    defprimitive('+', lambda args: args.car + args.cadr, 2)
    defprimitive('-', lambda args: args.car - args.cadr, 2)
    defprimitive('<', lambda args: args.car < args.cadr, 2)
    defprimitive('cons', lambda args: cons(args.car, args.cadr), 2)
    definitial('list', lambda args: args)
    definitial('fib', lambda args: args)
    while True:
        exprs = read()  # our version of read returns a list of expressions
        for expr in exprs:
            value = evaluate(expr, GLOBAL_ENV)
            print(value)


def evaluate(expr, env):
    if is_atom(expr):
        if is_symbol(expr):
            return lookup(expr, env)
        else:
            return expr
    else:
        first = expr.car
        if first == 'quote':
            return expr.cadr
        elif first == 'if':
            if evaluate(expr.cadr, env):
                return evaluate(expr.caddr, env)
            else:
                return evaluate(expr.cadddr, env)
        elif first == 'begin':
            return eprogn(expr.cdr, env)
        elif first == 'set!':
            return update(expr.cadr, env, evaluate(expr.caddr, env))
        elif first == 'lambda':
            return make_function(expr.cadr, expr.cddr, env)
        else:
            return invoke(evaluate(expr.car, env), evlis(expr.cdr, env))

def is_atom(expr):
    return not is_pair(expr)

def lookup(id, env):
    if is_pair(env):
        if env.caar == id:
            return env.cdar
        else:
            return lookup(id, env.cdr)
    raise ValueError("No such Binding: {}".format(id))

def update(id, env, value):
    if is_pair(env):
        if env.caar == id:
            env.cdar = value
            return value
        else:
            return update(id, env.cdr, value)
    raise ValueError("No such Binding: {}".format(id))

def extend(env, variables, values):
    if is_pair(variables):
        if is_pair(values):
            return cons(cons(variables.car, values.car),
                        extend(env, variables.cdr, values.cdr))
        else:
            raise ValueError("Too few values")
    elif is_null(variables):
        if is_null(values):
            return env
        else:
            raise ValueError("Too many values")
    elif is_symbol(variables):
        return cons(cons(variables, values), env)
    else:
        raise RuntimeError("Branch should be unreachable")

def eprogn(exps, env):
    if is_pair(exps):
        if is_pair(exps.cdr):
            evaluate(exps.car, env)
            return eprogn(exps.cdr, env)
        else:
            return evaluate(exps.car, env)
    else:
        return None

def evlis(exps, env):
    if is_pair(exps):
        argument = evaluate(exps.car, env)
        return cons(argument, evlis(exps.cdr, env))
    else:
        return Nil

def make_function(variables, body, env):
    return lambda values: eprogn(body, extend(env, variables, values))

def invoke(fn, args):
    if callable(fn):
        return fn(args)
    else:
        raise ValueError("Not a function: {}".format(fn))

GLOBAL_ENV = Nil

def definitial(name, value=None):
    global GLOBAL_ENV
    GLOBAL_ENV = cons(cons(name, value), GLOBAL_ENV)
    return name

def defprimitive(name, value, arity):
    return definitial(name, lambda values: primitive(value, values, arity))

def primitive(value, values, arity):
    if length(values) == arity:
        return value(values)
    else:
        raise ValueError("Incorrect arity")

if __name__ == '__main__':
    main()
