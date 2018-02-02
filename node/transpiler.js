// Transpile to JS
// How to use: node transpiler.js my-file.yl

const fs = require('fs');
const Parser = require('./parser');

const input = process.argv[2];
const code = fs.readFileSync(input, 'utf8');

const ast = Parser(code);

function codeGen(ast, scope={}, evaluateFunction=true, returnValue=false) {
    var returnString;
    if (returnValue) {
        returnString = 'return ';
    } else {
        returnString = '';
    }
    if (! (ast instanceof Array)) {
        if (scope[ast]) {
            return returnString + ast;;
        } else if (/^[0-9]+\.?[0-9]*$/.test(ast)) {
            return returnString + ast;
        } else {
            return `${returnString}"${ast}"`;
        }
    } else if (evaluateFunction && ast.length > 0 && !(ast[0] instanceof Array)) {
        switch (ast[0]) {
        case 'def':
            return returnString + genFunction(ast, scope);
        case 'print':
            return returnString + genPrintFn(ast.slice(1), scope);
        case 'let':
            return returnString + genLet(ast.slice(1), scope);
        case 'if':
            return returnString + genIf(ast.slice(1), scope);
        case 'loop':
            return returnString + genLoop(ast.slice(1), scope);
        case '+':
            return returnString + genPlusOp(ast.slice(1), scope);
        case '*':
            return returnString + genMulOp(ast.slice(1), scope);
        case '-':
            return returnString + genMinusOp(ast.slice(1), scope);
        case '/':
            return returnString + genDivOp(ast.slice(1), scope);
        case '%':
            return returnString + genModOp(ast.slice(1), scope);
        case '!':
            return returnString + genNotOp(ast.slice(1), scope);
        case '<':
            return returnString + genCmpOp(ast.slice(1), scope, '<');
        case '<=':
            return returnString + genCmpOp(ast.slice(1), scope, '<=');
        case '>':
            return returnString + genCmpOp(ast.slice(1), scope, '>');
        case '>=':
            return returnString + genCmpOp(ast.slice(1), scope, '>=');
        case '=':
            return returnString + genCmpOp(ast.slice(1), scope, '==');
        case 'argv':
            return returnString + genArgv(ast.slice(1), scope);
        case 'rand':
            return returnString + 'Math.random()';
        default:
            return returnString + genCallFn(ast, scope);
        }
    } else if (ast.length > 0) {
        var code = ast.slice(0, ast.length - 1).map(function (exp) {
            return codeGen(exp, scope) + ';';
        }).join('');
            code += returnString + codeGen(ast[ast.length - 1], scope);
        return code;
    } else {
        return 'undefined';
    }
}

function genFunction(ast, scope) {
    if (ast.length < 3) {
        throw 'def should have 3 arguments'
    }
    var identifier = ast[1];
    var parameter_names = ast[2];
    var fn_body = ast.slice(3);
    scope[identifier] = true;
    var fnScope = Object.create(scope);
    parameter_names.forEach(function (name) {
        fnScope[name] = true;
    });
    return `function ${identifier} (${parameter_names.join()}) { ${codeGen(fn_body, fnScope, false, true)} }`;
}

function genPrintFn(ast, scope) {
    return ast.map(function (exp) {
        return `(function () {
                   var __ret = ${codeGen(exp, scope)};
                   if (__ret === true) {
                     console.log(1);
                   } else if (__ret === false || __ret === undefined) {
                     console.log('()')
                   } else {
                     console.log(__ret)
                   }
                 })()`;
    }).join('');
}

function genCallFn(ast, scope) {
    args = ast.slice(1).map(function (exp) {
        return codeGen(exp, scope);
    }).join();
    return `${ast[0]}(${args})`;
}

function genLet(ast, scope) {
    var identifier = ast[0];
    var rhs = codeGen(ast[1], scope);
    scope[identifier] = true;
    return `var ${identifier} = ${rhs};`;
}

function genPlusOp(ast, scope) {
    return '(' + ast.map(function (exp) {
        return codeGen(exp, scope);
    }).join(' + ') + ')';
}

function genMulOp(ast, scope) {
    return '(' + ast.map(function (exp) {
        return codeGen(exp, scope);
    }).join(' * ') + ')';
}

function genMinusOp(ast, scope) {
    return '(' + ast.map(function (exp) {
        return codeGen(exp, scope);
    }).join(' - ') + ')';
}

function genDivOp(ast, scope) {
    return '(' + ast.map(function (exp) {
        return codeGen(exp, scope);
    }).join(' / ') + ')';
}

function genModOp(ast, scope) {
    return '(' + ast.map(function (exp) {
        return codeGen(exp, scope);
    }).join(' % ') + ')';
}

function genNotOp(ast, scope) {
    if (ast.length < 1) {
        throw '! should have 1 argument';
    } else {
        return `(!${codeGen(ast[0], scope)})`;
    }
}

function genCmpOp(ast, scope, op) {
  if (ast.length < 2) {
      throw `${op} should have 2 arguments`;
  } else {
      return `(${codeGen(ast[0], scope)} ${op} ${codeGen(ast[1], scope)})`;
  }
}

function genIf(ast, scope) {
    var cond = codeGen(ast[0], scope);
    var then = codeGen(ast[1], scope, false);
    var els
    if (ast.length > 2) {
        els = codeGen(ast[2], scope, false);
    } else {
        els = 'undefined';
    }
    return `(function () {
              var __cond = ${cond};
              if (__cond) {
                return ${then};
              } else {
                return ${els};
              }
            })()`;
}

function genLoop(ast, scope) {
    var identifier = ast[0];
    var loop;
    if (ast[1] instanceof Array) {
        if (ast[1].length < 2 || ast[1][0] !== 'range') {
            loop = '[' + ast[1].map(function (exp) {
                return codeGen(exp, scope);
            }).join() + ']';
        } else {
            var range = ast[1].slice(1);
            var min, max;
            if (range.length == 1) {
                min = '0';
                max = codeGen(range[0], scope);
            } else {
                min = codeGen(range[0], scope);
                max = codeGen(range[1], scope);
            }
            loop = `(function () {
              var __min = ${min};
              var __max = ${max};
              var __loop = [];
              while (__min < __max) {
                  __loop.push(__min);
                  __min += 1;
              }
              return __loop;
            })()`;
        }
    } else {
        loop = `[${codeGen(ast[1], scope)}]`;
    }
    scope[identifier] = true;
    return `(function () {
              var __ret;
              for (${identifier} of ${loop}) {
                __ret = (function () {
                  ${codeGen(ast.slice(2), scope, false, true)}
                })();
              }
              return __ret;
            })()`
}

function genArgv(ast, scope) {
    if (ast.length < 1) {
        throw "'argv' expects 1 argument!";
    }
    var i = codeGen(ast[0], scope);
    return `process.argv[(${i}) + 2]`;
}

// Output to stdout
console.log(codeGen(ast));
