import shlex


def read(prompt="lisp1>>"):
    string = input(prompt)
    tokens = tokenize(string)
    return parse(tokens)


class Nil: pass
class Parenthesis: pass
class Quote: pass
class Symbol(str): pass


class Pair:
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        return 'Pair({}, {})'.format(repr(self.car), repr(self.cdr))

    def __str__(self):
        return self.list_to_str().join(['(', ')'])

    def list_to_str(self):
        if isinstance(self.cdr, Pair):
            return '{} {}'.format(self.car, self.cdr.list_to_str())
        if self.cdr == Nil:
            return '{}'.format(self.car)
        return '{} . {}'.format(self.car, self.cdr)

    def __getattr__(self, item):
        if item[0] == 'c' and item[-1] == 'r':
            value = self
            for x in item[-2:0:-1]:
                if x == 'a':
                    value = value.car
                elif x == 'd':
                    value = value.cdr
                else:
                    raise AttributeError(item)
            return value
        raise AttributeError(item)


class String(str):
    def __str__(self):
        return self.join(['"', '"'])


def tokenize(string):
    return shlex.split(string.replace("'", " '' ").replace('(', ' ( ').replace(')', ' ) '), posix=False)


def parse(tokens):
    stack = []

    for token in tokens:
        if token == '(':
            stack.append(Parenthesis)
        elif token == ')':
            datum = Nil
            while True:
                top = stack.pop()
                if top == Parenthesis:
                    break
                elif top == '.':
                    if datum.cdr is not Nil:
                        raise SyntaxError("too many tokens after .")
                    datum = cons(stack.pop(), datum.car)
                else:
                    datum = cons(top, datum)
            stack.append(datum)
        elif token == '#t':
            stack.append(True)
        elif token == '#f':
            stack.append(False)
        elif token == "''":
            stack.append(Quote)
        elif token.startswith('"') and token.endswith('"'):
            stack.append(String(token[1:-1]))
        else:
            try:
                stack.append(int(token))
            except ValueError:
                try:
                    stack.append(float(token))
                except ValueError:
                    stack.append(Symbol(token))
        if len(stack) >= 2 and stack[-2] == Quote and stack[-1] != Parenthesis:
            quoted = stack.pop()
            stack.pop()
            stack.append(cons(Symbol("quote"), cons(quoted, Nil)))

    return stack

def length(x):
    if is_pair(x):
        return 1 + length(x.cdr)
    else:
        return 0

def is_atom(expr):
    return not is_pair(expr)

def is_null(x):
    return x is Nil or isinstance(x, Nil)

def is_symbol(x):
    return isinstance(x, Symbol)

def is_pair(x):
    return isinstance(x, Pair)

def cons(car, cdr):
    return Pair(car, cdr)

def car(x):
    return x.car

def cdr(x):
    return x.cdr

def is_eqv(a, b):
    if isinstance(a, Pair) and isinstance(b, Pair):
        return a is b
    return a == b

def is_eq(a, b):
    return a is b
