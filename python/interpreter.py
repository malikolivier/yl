import sys
import random
import re


YL_FALSE = None
YL_TRUE = 1


class InterpreterException(Exception):
    pass


def print_fn(args, scope):
    obj = args[0]
    if obj is YL_FALSE:
        print('()')
    else:
        if type(obj) is float and abs(round(obj) - obj) < 0.00000001:
            print(round(obj))
        else:
            print(obj)


def not_op(args, scope):
    a = args[0]
    if a is YL_FALSE:
        return YL_TRUE
    else:
        return YL_FALSE


def eq_op(args, scope):
    a, b = args[0], args[1]
    if a == b:
        return YL_TRUE
    else:
        return YL_FALSE


def gt_op(args, scope):
    a, b = args[0], args[1]
    if a > b:
        return YL_TRUE
    else:
        return YL_FALSE


def ge_op(args, scope):
    a, b = args[0], args[1]
    if a >= b:
        return YL_TRUE
    else:
        return YL_FALSE


def lt_op(args, scope):
    a, b = args[0], args[1]
    if a < b:
        return YL_TRUE
    else:
        return YL_FALSE


def le_op(args, scope):
    a, b = args[0], args[1]
    if a <= b:
        return YL_TRUE
    else:
        return YL_FALSE


def plus_op(args, scope):
    any_string = False
    for arg in args:
        if type(arg) is str:
            any_string = True
    if any_string:
        ret = ''
        for arg in args:
            ret += str(arg)
    else:
        ret = 0
        for arg in args:
            ret += arg
    return ret


def minus_op(args, scope):
    return args[0] - args[1]


def multiply_op(args, scope):
    return args[0] * args[1]


def divide_op(args, scope):
    return args[0] / args[1]


def modulo_op(args, scope):
    return args[0] % args[1]


def let_fn(args, scope):
    identifier, rhs = args[0], args[1]
    scope.set(identifier, rhs)
    return rhs


def if_fn(args, scope):
    cond = args[0]
    if cond is YL_FALSE:
        if len(args) > 2:
            return evaluate(args[2], scope, evaluate_function=False)
        else:
            return YL_FALSE
    else:
        return evaluate(args[1], scope, evaluate_function=False)


def loop_fn(args, scope):
    ret = YL_FALSE
    identifier, values, exp = args
    if values[0] == 'range':
        if len(values) == 2:
            mini = 0
            maxi = float(evaluate(values[1], scope))
        else:
            mini = float(evaluate(values[1], scope))
            maxi = float(evaluate(values[2], scope))
        values = []
        i = mini
        while i < maxi:
            values.append(i)
            i += 1
    loop_scope = scope.extend()
    for value in values:
        loop_scope.set(identifier, value)
        ret = evaluate(exp, loop_scope)
    return ret


def def_fn(args, scope):
    identifier, arg_names, exps = args[0], args[1], args[2:]
    def rhs(argv, _scope):
        fn_scope = scope.extend()
        for i, name in enumerate(arg_names):
            if i < len(argv):
                fn_scope.set(name, argv[i])
            else:
                fn_scope.set(name, YL_FALSE)
        return evaluate(exps, fn_scope, evaluate_function=False)
    scope.set(identifier, rhs)
    return rhs


def argv_fn(args, scope):
    n = int(args[0]) + 1
    if n < len(sys.argv):
        if is_number(sys.argv[n]):
            return float(sys.argv[n])
        else:
            return sys.argv[n]
    else:
        return YL_FALSE


def rand_fn(args, scope):
    return random.random()


GLOBAL_VARS = {
    'print': print_fn,
    '!': not_op,
    '=': eq_op,
    '>': gt_op,
    '<': lt_op,
    '>=': ge_op,
    '<=': le_op,
    '+': plus_op,
    '-': minus_op,
    '*': multiply_op,
    '/': divide_op,
    '%': modulo_op,
    'let': let_fn,
    'if': if_fn,
    'loop': loop_fn,
    'def': def_fn,
    'argv': argv_fn,
    'rand': rand_fn
}

class Scope:
    def __init__(self, parent=None):
        self.vars = GLOBAL_VARS if parent is None else {}
        self.parent = parent

    def extend(self):
        return Scope(self)

    def get(self, name):
        parent = self
        while parent is not None:
            if name in parent.vars:
                return parent.vars[name]
            parent = parent.parent
        raise InterpreterException('Undefined variable %s' % name);

    def __contains__(self, name):
        parent = self
        while parent is not None:
            if name in parent.vars:
                return True
            parent = parent.parent
        return False

    def set(self, name, value):
        self.vars[name] = value
        return value


def is_number(string):
    return re.match(r'^[0-9]+\.?[0-9]*$', string)


def evaluate(exp, scope=Scope(), evaluate_function=True):
    #print(exp)
    if type(exp) is not list:
        try:
            return scope.get(exp)
        except InterpreterException:
            if is_number(exp):
                return float(exp)
            else:
                return exp
    elif evaluate_function and len(exp) > 0 and type(exp[0]) is not list and exp[0] in scope:
        if exp[0] == 'def':
            return scope.get('def')(exp[1:], scope)
        elif exp[0] == 'if':
            cond = evaluate(exp[1], scope)
            args = [cond]
            for subexp in exp[2:]:
                args.append(subexp)
            return scope.get('if')(args, scope)
        elif exp[0] == 'loop':
            scope.get('loop')(exp[1:], scope)
        else:
            # Run a function
            args = [evaluate(subexp, scope) for subexp in exp[1:]]
            return scope.get(exp[0])(args, scope)
    else:
        ret = YL_FALSE
        for subexp in exp:
            ret = evaluate(subexp, scope)
        return ret
