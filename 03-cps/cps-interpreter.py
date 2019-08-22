import sys
sys.path.append("../libs")
from utils import read, is_atom, is_eqv, is_null, is_symbol, is_pair, car, cons, length, Pair, Symbol, Nil

# I'm writing this interpreter in Python instead of Scheme because I do not
# have an object system implementation readily available.


def generic_function(default):
    def inner(obj, *args, **kwargs):
        try:
            method = getattr(obj, default.__name__)
        except AttributeError:
            return default(obj, *args, **kwargs)
        return method(*args, **kwargs)
    return inner



@generic_function
def invoke(f, vs, r, k):
    raise TypeError("Not a function {} {} {}".format(f, r, k))
    
@generic_function
def resume(k, v):
    raise TypeError("Unknown continuation {}".format(k))
    
@generic_function
def lookup(r, n, k):
    raise TypeError("Not an environment {} {} {}".format(r, n, k))
    
@generic_function
def update(r, n, k, v):
    raise TypeError("Not an environment {} {} {}".format(r, n, k))
        
class Value: pass

class Environment: pass

class Continuation:
    def __init__(self, k):
        self.k = k
        
    def invoke(self, vs, r, k):
        if length(vs) == 1:
            return resume(self, vs.car)
        else:
            raise TypeError("Continuations expect one argument {} {} {}".format(vs, r, k))
    
    
def evaluate(e, r, k):
    if is_atom(e):
        if is_symbol(e): return evaluate_variable(e, r, k)
        else: return evaluate_quote(e, r, k)
    else:
        first = e.car
        if first == 'quote': return evaluate_quote(e.cadr, r, k)
        elif first == 'if': return evaluate_if(e.cadr, e.caddr, e.cadddr, r, k)
        elif first == 'begin': return evaluate_begin(e.cdr, r, k)
        elif first == 'set!': return evaluate_set(e.cadr, e.caddr, r, k)
        elif first == 'lambda': return evaluate_lambda(e.cadr, e.cddr, r, k)
        else: return evaluate_application(e.car, e.cdr, r, k)
        

def evaluate_quote(v, r, k):
    return resume(k, v)


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
        
    def resume(self, v):
        return evaluate_begin(self.es.cdr, self.r, self.k)
        

def evaluate_begin(es, r, k):
    if is_pair(es):
        if is_pair(es.cdr):
            return evaluate(es.car, r, BeginCont(k, es, r))
        else:
            return evaluate(es.car, r, k)
    else:
        return resume(k, None)
    
    
class NullEnv(Environment):
    def __init__(self):
        super().__init__()
        
    def lookup(self, n, k):
        raise NameError("Unknown variable {} {} {}".format(n, self, k))
        
    def update(self, n, k, v):
        raise NameError("Unknown variable {} {} {}".format(n, self, k))

class FullEnv(Environment):
    def __init__(self, others, name):
        super().__init__()
        self.others = others
        self.name = name
        
    def lookup(self, n, k):
        return lookup(self.others, n, k)
        
    def update(self, n, k, v):
        return update(self.others, n, k, v)
        
class VariableEnv(FullEnv):
    def __init__(self, others, name, value):
        super().__init__(others, name)
        self.value = value
        
    def lookup(self, n, k):
        if is_eqv(n, self.name):
            return resume(k, self.value)
        else:
            return lookup(self.others, n, k)
        
    def update(self, n, k, v):
        if is_eqv(n, self.name):
            self.value = v
            return resume(k, v)
        else:
            return update(self.others, n, k, v)
        

def evaluate_variable(n, r, k):
    return lookup(r, n, k)


class SetCont(Continuation):
    def __init__(self, k, n, r):
        super().__init__(k)
        self.n = n
        self.r = r
        
    def resume(self, v):
        update(self.r, self.n, self.k, v)
                
def evaluate_set(n, e, r, k):
    return evaluate(e, r, SetCont(k, n, r))


class Function(Value):
    def __init__(self, variables, body, env):
        super().__init__()
        self.variables = variables
        self.body = body
        self.env = env
        
    def invoke(self, vs, r, k):
        env = extend_env(self.env, self.variables, vs)
        return evaluate_begin(self.body, env, k)
        
def evaluate_lambda(ns, es, r, k):
    return resume(k, Function(ns, es, r))

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
        return evaluate_arguments(self.es, self.r, ApplyCont(self.k, f, self.r))
        
        
class ApplyCont(Continuation):      
    def __init__(self, k, f, r):
        super().__init__(k)
        self.f = f
        self.r = r
        
    def resume(self, v):
        return invoke(self.f, v, self.r, self.k)
        
        
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
        return resume(self.k, cons(self.v, vs))
        
        
def evaluate_application(e, es, r, k):
    return evaluate(e, r, EvFunCont(k, es, r))

def evaluate_arguments(es, r, k, no_more_arguments=Nil):
    if is_pair(es):
        return evaluate(es.car, r, ArgumentCont(k, es, r))
    else:
        return resume(k, no_more_arguments)


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
    return definitial(name, Primitive(name, lambda vs, r, k: primitive(name, value, vs, arity, k)))

def primitive(name, value, vs, arity, k):
    if arity == length(vs):
        return resume(k, value(vs))
    else:
        raise TypeError("incorrect arity {} {}".format(name, vs))

        
GLOBAL_ENV = Nil
definitial('f')
defprimitive('cons', lambda args: cons(args.car, args.cadr), 2)
defprimitive('car', lambda args: args.caar, 1)
defprimitive('cdr', lambda args: args.cdar, 1)

definitial(
    'call/cc', 
    Primitive(
        'call/cc', 
        lambda vs, r, k: invoke(vs.car, cons(k, Nil), r, k) if length(vs) == 1 else wrong(TypeError("incorrect arity {} {}".format('call/cc', vs)))))
           
            
def wrong(e):
    raise e


class BottomCont(Continuation):
    def __init__(self, f):
        super().__init__(None)
        self.f = f
        
    def resume(self, v):
        return self.f(v)
    
def chapter3_interpreter():
    while True:
        for e in read("CPS>>"):
            evaluate(e, GLOBAL_ENV, BottomCont(print))
    

if __name__ == '__main__':
    chapter3_interpreter()
    