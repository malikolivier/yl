const YL_FALSE = null;
const YL_TRUE = 1;

GLOBAL_VARS = {
    ['def'] (identifier, arg_names, exps, scope) {
        function rhs () {
            var fn_scope = scope.extend();
            for (var i = 0; i < arguments.length - 1; i++) {
                fn_scope.set(arg_names[i], i < arguments.length - 1 ? arguments[i] : YL_FALSE);
            }
            return evaluate(exps, fn_scope, false);
        }
        scope.set(identifier, rhs);
        return rhs;
    },
    ['let'] (identifier, exp, scope) {
        rhs = exp;
        scope.set(identifier, rhs);
        return rhs;
    },
    ['print'] (obj, scope) {
        var out = obj;
        if (out === YL_FALSE) {
            console.log('()')
        } else {
            console.log(out);
        }
        return YL_FALSE;
    },
    ['!'] (a, scope) {
        if (a === YL_FALSE) {
            return YL_TRUE;
        } else {
            return YL_FALSE;
        }
    },
    ['='] (a, b, scope) {
        if (a === b) {
            return YL_TRUE;
        } else {
            return YL_FALSE;
        }
    },
    ['>'] (a, b, scope) {
        if (a > b) {
            return YL_TRUE;
        } else {
            return YL_FALSE;
        }
    },
    ['<'] (a, b, scope) {
        if (a < b) {
            return YL_TRUE;
        } else {
            return YL_FALSE;
        }
    },
    ['>='] (a, b, scope) {
        if (a >= b) {
            return YL_TRUE;
        } else {
            return YL_FALSE;
        }
    },
    ['<='] (a, b, scope) {
        if (a <= b) {
            return YL_TRUE;
        } else {
            return YL_FALSE;
        }
    },
    ['+'] (a, b, scope) {
        return a + b;
    },
    ['-'] (a, b, scope) {
        return a - b;
    },
    ['*'] (a, b, scope) {
        return a * b;
    },
    ['/'] (a, b, scope) {
        return a / b;
    },
    ['%'] (a, b, scope) {
        return a % b;
    },
    ['if'] (cond, exp1, exp2, scope) {
        if (cond === YL_FALSE) {
            if (exp2) {
                return evaluate(exp2, scope, false);
            } else {
                return YL_FALSE;
            }
        } else {
            return evaluate(exp1, scope, false);
        }
    },
    ['loop'] (identifier, values, exp, scope) {
        var ret = YL_FALSE;
        if (values[0] === 'range') {
            var min, max;
            if (values.length === 2) {
                min = 0;
                max = evaluate(values[1], scope);
            } else {
                min = evaluate(values[1], scope);
                max = evaluate(values[2], scope);
            }
            values = [];
            for (var i = min; i < max; i++) {
                values.push(i);
            }
        }
        var loop_scope = scope.extend();
        for (var i = 0; i < values.length; i++) {
            loop_scope.set(identifier, values[i]);
            ret = evaluate(exp, loop_scope);
        }
        return ret;
    },
    ['argv'] (n, scope) {
        return process.argv[n + 3];
    },
    ['rand'] (scope) {
        return Math.random();
    }
};

function Scope(parent) {
    this.vars = Object.create(parent ? parent.vars : GLOBAL_VARS);
    this.parent = parent;
}

Scope.prototype = {
    extend() {
      return new Scope(this);
    },
    get(name) {
        if (name in this.vars)
            return this.vars[name];
        throw new Error("Undefined variable " + name);
    },
    set(name, value) {
        return this.vars[name] = value;
    }
}

function evaluate(exp, scope, evaluate_function=true) {
    if (!scope) {
        scope = new Scope(null); // Global scope
    }
    if (! (exp instanceof Array)) {
        try {
            return scope.get(exp);
        } catch (_) {
            if (/^[0-9]+\.?[0-9]*$/.test(exp)) {
                // This is a number!
                return Number(exp);
            } else {
                return exp;
            }
        }
    } else if (evaluate_function && scope.vars[exp[0]]) {
        if (exp[0] === 'def') {
            // Create a funtion
            var identifier = exp[1];
            var arg_names = exp[2];
            var fn_body = [];
            for (var i = 3; i < exp.length; i++) {
                fn_body.push(exp[i]);
            }
            return scope.vars['def'](identifier, arg_names, fn_body, scope);
        } else if (exp[0] === 'if') {
            // Do not directly evaluate if's arguments! Use lazy evaluation
            var cond = evaluate(exp[1], scope);
            return scope.vars['if'](cond, exp[2], exp[3], scope);
        } else if (exp[0] === 'loop') {
            return scope.vars['loop'](exp[1], exp[2], exp[3], scope);
        } else {
            // Run a function
            var args = []
            for (var i = 1; i < exp.length; i++) {
                args.push(evaluate(exp[i], scope));
            }
            args.push(scope);
            return scope.vars[exp[0]].apply(null, args);
        }
    } else {
        var ret = YL_FALSE;
        exp.forEach(function (subexp) {
            ret = evaluate(subexp, scope);
        });
        return ret;
    }
}


module.exports = {
    Interpreter: evaluate,
    Scope
};
