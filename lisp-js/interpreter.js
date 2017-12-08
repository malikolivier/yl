const YL_FALSE = null;
const YL_TRUE = 1;

GLOBAL_VARS = {
    ['def'] (identifier, arg_names, exps, scope) {
        function rhs () {
            var fn_scope = scope.extend();
            arg_names.forEach(function (name, i) {
                fn_scope.set(name, i < arguments.length ? arguments[i] : YL_FALSE);
            });
            return evaluate(exps, fn_scope);
        }
        scope.set(identifier, rhs);
        return rhs;
    },
    ['let'] (identifier, exp, scope) {
        rhs = evaluate(exp, scope);
        scope.set(identifier, rhs);
        return rhs;
    },
    ['print'] (obj, scope) {
        console.log(evaluate(obj, scope));
        return YL_FALSE;
    },
    ['='] (a, b, scope) {
        if (evaluate(a, scope) === evaluate(b, scope)) {
            return YL_TRUE;
        } else {
            return YL_FALSE;
        }
    },
    ['+'] (a, b, scope) {
        return evaluate(a, scope) + evaluate(b, scope);
    },
    ['-'] (a, b, scope) {
        return evaluate(a, scope) - evaluate(b, scope);
    },
    ['*'] (a, b, scope) {
        return evaluate(a, scope) * evaluate(b, scope);
    },
    ['/'] (a, b, scope) {
        return evaluate(a, scope) / evaluate(b, scope);
    },
    ['%'] (a, b, scope) {
        return evaluate(a, scope) % evaluate(b, scope);
    },
    ['if'] (cond, exp1, exp2, scope) {
        var ret = evaluate(cond, scope);
        if (ret === YL_FALSE) {
            return evaluate(exp2, scope);
        } else {
            return evaluate(exp1, scope);
        }
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

function evaluate(exp, scope) {
    if (!scope) {
        scope = new Scope(null); // Global scope
    }
    if (! (exp instanceof Array)) {
        return exp;
    } else if (scope.vars[exp[0]]) {
        // Run a function
        var args = []
        for (var i = 1; i < exp.length; i++) {
            args.push(evaluate(exp[i], scope));
        }
        args.push(scope);
        return scope.vars[exp[0]].apply(null, args);
    } else {
        var ret = YL_FALSE;
        exp.forEach(function (subexp) {
            ret = evaluate(subexp, scope);
        });
        return ret;
    }
}


module.exports = evaluate;
