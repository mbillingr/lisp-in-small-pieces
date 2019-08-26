import sys

sys.path.append("../libs")
from utils import read, is_atom, is_eq, is_eqv, is_null, is_symbol, is_pair, \
    cons, length, Symbol, Nil


# I'm writing this interpreter in Python instead of Scheme because I do not
# have an object system implementation readily available.


class Value:
    pass


class Environment:
    pass


class Continuation:
    def __init__(self, k):
        self.k = k

    def invoke(self, vs, r, k):
        if length(vs) == 1:
            return self.resume(vs.car)
        else:
            raise TypeError(
                "Continuations expect one argument {} {} {}".format(vs, r, k))

    def catch_lookup(self, tag, kk):
        return self.k.catch_lookup(tag, kk)

    def unwind(self, v, ktarget):
        if is_eq(self, ktarget):
            ktarget.resume(v)
        else:
            self.k.unwind(v, ktarget)


def evaluate(e, r, k):
    if is_atom(e):
        if is_symbol(e):
            return evaluate_variable(e, r, k)
        else:
            return evaluate_quote(e, r, k)
    else:
        first = e.car
        if first == 'quote':
            return evaluate_quote(e.cadr, r, k)
        elif first == 'if':
            return evaluate_if(e.cadr, e.caddr, e.cadddr, r, k)
        elif first == 'begin':
            return evaluate_begin(e.cdr, r, k)
        elif first == 'set!':
            return evaluate_set(e.cadr, e.caddr, r, k)
        elif first == 'lambda':
            return evaluate_lambda(e.cadr, e.cddr, r, k)
        elif first == 'catch':
            return evaluate_catch(e.cadr, e.cddr, r, k)
        elif first == 'throw':
            return evaluate_throw(e.cadr, e.caddr, r, k)
        elif first == 'block':
            return evaluate_block(e.cadr, e.cddr, r, k)
        elif first == 'return-from':
            return evaluate_return_from(e.cadr, e.caddr, r, k)
        elif first == 'unwind-protect':
            return evaluate_unwind_protect(e.cadr, e.cddr, r, k)
        else:
            return evaluate_application(e.car, e.cdr, r, k)


def evaluate_quote(v, _r, k):
    return k.resume(v)


class IfCont(Continuation):
    def __init__(self, k, et, ef, r):
        super().__init__(k)
        self.et = et
        self.ef = ef
        self.r = r

    def resume(self, v):
        return evaluate(self.et if v else self.ef, self.r, self.k)


def evaluate_if(ec, et, ef, r, k):
    return evaluate(ec, r, IfCont(k, et, ef, r))


class BeginCont(Continuation):
    def __init__(self, k, es, r):
        super().__init__(k)
        self.es = es
        self.r = r

    def resume(self, _v):
        return evaluate_begin(self.es.cdr, self.r, self.k)


def evaluate_begin(es, r, k):
    if is_pair(es):
        if is_pair(es.cdr):
            return evaluate(es.car, r, BeginCont(k, es, r))
        else:
            return evaluate(es.car, r, k)
    else:
        return k.resume(None)


class NullEnv(Environment):
    def __init__(self):
        super().__init__()

    def lookup(self, n, k):
        raise NameError("Unknown variable {} {} {}".format(n, self, k))

    def update(self, n, k, v):
        raise NameError("Unknown variable {} {} {}".format(n, self, k))

    def block_lookup(self, n, k, v):
        raise NameError(
            "Unknown block label {} {} {} {}".format(n, self, k, v))


class FullEnv(Environment):
    def __init__(self, others, name):
        super().__init__()
        self.others = others
        self.name = name

    def lookup(self, n, k):
        return self.others.lookup(n, k)

    def update(self, n, k, v):
        return self.others.update(n, k, v)

    def block_lookup(self, n, k, v):
        return self.others.block_lookup(n, k, v)


class VariableEnv(FullEnv):
    def __init__(self, others, name, value):
        super().__init__(others, name)
        self.value = value

    def lookup(self, n, k):
        if is_eqv(n, self.name):
            return k.resume(self.value)
        else:
            return self.others.lookup(n, k)

    def update(self, n, k, v):
        if is_eqv(n, self.name):
            self.value = v
            return k.resume(v)
        else:
            return self.others.update(n, k, v)


def evaluate_variable(n, r, k):
    return r.lookup(n, k)


class SetCont(Continuation):
    def __init__(self, k, n, r):
        super().__init__(k)
        self.n = n
        self.r = r

    def resume(self, v):
        self.r.update(self.n, self.k, v)


def evaluate_set(n, e, r, k):
    return evaluate(e, r, SetCont(k, n, r))


class Function(Value):
    def __init__(self, variables, body, env):
        super().__init__()
        self.variables = variables
        self.body = body
        self.env = env

    def invoke(self, vs, _r, k):
        env = extend_env(self.env, self.variables, vs)
        return evaluate_begin(self.body, env, k)


def evaluate_lambda(ns, es, r, k):
    return k.resume(Function(ns, es, r))


def extend_env(env, names, values):
    if is_pair(names) and is_pair(values):
        return VariableEnv(extend_env(env, names.cdr, values.cdr),
                           names.car,
                           values.car)
    elif is_null(names) and is_null(values):
        return env
    elif is_symbol(names):
        return VariableEnv(env, names, values)
    else:
        raise TypeError("Arity mismatch")


class EvFunCont(Continuation):
    def __init__(self, k, es, r):
        super().__init__(k)
        self.es = es
        self.r = r

    def resume(self, f):
        return evaluate_arguments(self.es, self.r,
                                  ApplyCont(self.k, f, self.r))


class ApplyCont(Continuation):
    def __init__(self, k, f, r):
        super().__init__(k)
        self.f = f
        self.r = r

    def resume(self, v):
        return self.f.invoke(v, self.r, self.k)


class ArgumentCont(Continuation):
    def __init__(self, k, es, r):
        super().__init__(k)
        self.es = es
        self.r = r

    def resume(self, v):
        return evaluate_arguments(self.es.cdr, self.r, GatherCont(self.k, v))


class GatherCont(Continuation):
    def __init__(self, k, v):
        super().__init__(k)
        self.v = v

    def resume(self, vs):
        return self.k.resume(cons(self.v, vs))


def evaluate_application(e, es, r, k):
    return evaluate(e, r, EvFunCont(k, es, r))


def evaluate_arguments(es, r, k, no_more_arguments=Nil):
    if is_pair(es):
        return evaluate(es.car, r, ArgumentCont(k, es, r))
    else:
        return k.resume(no_more_arguments)


# ------ Implementation of catch / throw ------


class CatchCont(Continuation):
    def __init__(self, k, body, r):
        super().__init__(k)
        self.body = body
        self.r = r

    def resume(self, v):
        return evaluate_begin(self.body, self.r, LabeledCont(self.k, v))


class LabeledCont(Continuation):
    def __init__(self, k, tag):
        super().__init__(k)
        self.tag = tag

    def resume(self, v):
        return self.k.resume(v)

    def catch_lookup(self, tag, kk):
        if is_eqv(tag, self.tag):
            return evaluate(kk.form, kk.r, ThrowingCont(kk, tag, self))
        else:
            return self.k.catch_lookup(tag, kk)


def evaluate_catch(tag, body, r, k):
    return evaluate(tag, r, CatchCont(k, body, r))


class ThrowCont(Continuation):
    def __init__(self, k, form, r):
        super().__init__(k)
        self.form = form
        self.r = r

    def resume(self, tag):
        return self.catch_lookup(tag, self)


class ThrowingCont(Continuation):
    def __init__(self, k, tag, cont):
        super().__init__(k)
        self.tag = tag
        self.cont = cont

    def resume(self, v):
        return self.k.unwind(v, self.cont)


def evaluate_throw(tag, form, r, k):
    return evaluate(tag, r, ThrowCont(k, form, r))


# ------ Implementation of block / return-from ------


class BlockCont(Continuation):
    def __init__(self, k, label):
        super().__init__(k)
        self.label = label

    def resume(self, v):
        return self.k.resume(v)


class BlockEnv(FullEnv):
    def __init__(self, others, name, cont):
        super().__init__(others, name)
        self.cont = cont

    def block_lookup(self, n, k, v):
        if is_eq(n, self.name):
            return k.unwind(v, self.cont)
        else:
            return self.others.block_lookup(n, k, v)


def evaluate_block(label, body, r, k):
    k = BlockCont(k, label)
    return evaluate_begin(body, BlockEnv(r, label, k), k)


class ReturnFromCont(Continuation):
    def __init__(self, k, r, label):
        super().__init__(k)
        self.r = r
        self.label = label

    def resume(self, v):
        return self.r.block_lookup(self.label, self.k, v)


def evaluate_return_from(label, form, r, k):
    return evaluate(form, r, ReturnFromCont(k, r, label))


# ------ Implementation of unwind-protect ------


class UnwindCont(Continuation):
    def __init__(self, k, value, target):
        super().__init__(k)
        self.value = value
        self.target = target

    def resume(self, _v):
        self.k.unwind(self.value, self.target)


class UnwindProtectCont(Continuation):
    def __init__(self, k, cleanup, r):
        super().__init__(k)
        self.r = r
        self.cleanup = cleanup

    def unwind(self, v, target):
        return evaluate_begin(self.cleanup, self.r,
                              UnwindCont(self.k, v, target))


class ProtectReturnCont(Continuation):
    def __init__(self, k, value):
        super().__init__(k)
        self.value = value

    def resume(self, _v):
        self.k.resume(self.value)


def evaluate_unwind_protect(form, cleanup, r, k):
    return evaluate(form, r, UnwindProtectCont(k, cleanup, r))


# ------ Primitives and REPL ------


def definitial(name, value=None):
    global GLOBAL_ENV
    GLOBAL_ENV = VariableEnv(GLOBAL_ENV, Symbol(name), value)
    return name


class Primitive(Value):
    def __init__(self, name, address):
        super().__init__()
        self.name = name
        self.address = address

    def invoke(self, vs, r, k):
        return self.address(vs, r, k)


def defprimitive(name, value, arity):
    return definitial(name, Primitive(name,
                                      lambda vs, r, k: primitive(name, value,
                                                                 vs, arity,
                                                                 k)))


def primitive(name, value, vs, arity, k):
    if arity == length(vs):
        return k.resume(value(vs))
    else:
        raise TypeError("incorrect arity {} {}".format(name, vs))


GLOBAL_ENV = NullEnv()
definitial('f')
definitial('foo')
definitial('bar')
definitial('x')
definitial('y')
definitial('z')
defprimitive('cons', lambda args: cons(args.car, args.cadr), 2)
defprimitive('car', lambda args: args.caar, 1)
defprimitive('cdr', lambda args: args.cdar, 1)
defprimitive('+', lambda args: args.car + args.cadr, 2)
defprimitive('-', lambda args: args.car - args.cadr, 2)
defprimitive('*', lambda args: args.car * args.cadr, 2)
defprimitive('/', lambda args: args.car / args.cadr, 2)
definitial('println', Primitive('println', lambda args, r, k: k.resume(
    print() if is_null(args) else print(str(args)[1:-1]))))

definitial(
    'call/cc',
    Primitive(
        'call/cc',
        lambda vs, r, k: vs.car.invoke(cons(k, Nil), r, k) if length(
            vs) == 1 else wrong(
            TypeError("incorrect arity {} {}".format('call/cc', vs)))))


def wrong(e):
    raise e


class BottomCont(Continuation):
    def __init__(self, f):
        super().__init__(None)
        self.f = f

    def resume(self, v):
        return self.f(v)

    def catch_lookup(self, tag, kk):
        raise ValueError("No associated catch {} {} {}".format(self, tag, kk))

    def unwind(self, v, ktarget):
        raise ValueError("Obsolete continuation {}".format(v))


def chapter3_interpreter():
    while True:
        for e in read("CPS>>"):
            evaluate(e, GLOBAL_ENV, BottomCont(print))


if __name__ == '__main__':
    chapter3_interpreter()
