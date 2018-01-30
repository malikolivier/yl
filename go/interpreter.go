package main

import (
	"fmt"
	"math"
	"strconv"
)

type Scope struct {
	parent *Scope
	vars   map[string]Var
}

func createParentScope() Scope {
	return Scope{nil, map[string]Var{
		"print": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			for _, arg := range args {
				fmt.Println(varToString(arg))
			}
			return newVarFalse()
		}},
		"!": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 1 {
				panic("'!' function expects 1 argument!")
			}
			return varFromBool(args[0].kind == VarFalse)
		}},
		"let": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 1 {
				panic("'let' function should be used as '(let name val)'!")
			}
			identifier := varToString(args[0])
			rhs := newVarFalse()
			if len(args) > 1 {
				rhs = args[1]
			}
			scopeSet(scope, identifier, rhs)
			return rhs
		}},
		"def": Var{VarFunc, 0, "", nil},
		"=": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 2 {
				panic("'=' function expects 2 arguments!")
			}
			return varFromBool(args[0].eq(args[1]))
		}},
		">": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 2 {
				panic("'>' function expects 2 arguments!")
			}
			return varFromBool(args[0].gt(args[1]))
		}},
		">=": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 2 {
				panic("'>=' function expects 2 arguments!")
			}
			return varFromBool(args[0].ge(args[1]))
		}},
		"<": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 2 {
				panic("'<' function expects 2 arguments!")
			}
			return varFromBool(args[0].lt(args[1]))
		}},
		"<=": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 2 {
				panic("'<=' function expects 2 arguments!")
			}
			return varFromBool(args[0].le(args[1]))
		}},
		"+": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 1 {
				panic("'+' function expects at least 1 argument!")
			}
			ret := args[0]
			for _, arg := range args[1:] {
				ret = ret.add(arg)
			}
			return ret
		}},
		"-": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 1 {
				panic("'-' function expects at least 1 argument!")
			}
			if len(args) == 1 {
				arg0 := Var{VarNum, 0, "", nil}
				return arg0.sub(args[0])
			} else {
				return args[0].sub(args[1])
			}
		}},
		"*": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 2 {
				panic("'*' function expects 2 arguments!")
			}
			return args[0].mul(args[1])
		}},
		"/": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 2 {
				panic("'/' function expects 2 arguments!")
			}
			return args[0].div(args[1])
		}},
		"%": Var{VarFunc, 0, "", func(args []Var, scope *Scope) Var {
			if len(args) < 2 {
				panic("'%' function expects 2 arguments!")
			}
			return args[0].mod(args[1])
		}}}}
}

func scopeGet(scope *Scope, key string) (Var, bool) {
	v, ok := scope.vars[key]
	if ok {
		return v, true
	} else if scope.parent != nil {
		return scopeGet(scope.parent, key)
	} else {
		var zeroValue Var
		return zeroValue, false
	}
}

func scopeSet(scope *Scope, key string, v Var) {
	scope.vars[key] = v
}

func scopeExtend(scope *Scope) Scope {
	return Scope{scope, map[string]Var{}}
}

const (
	VarFalse = iota
	VarNum
	VarStr
	VarFunc
)

type Var struct {
	kind int
	num  float64
	str  string
	fn   func([]Var, *Scope) Var
}

func newVarFalse() Var {
	return Var{VarFalse, 0, "", nil}
}

func varFromBool(b bool) Var {
	if b {
		return Var{VarNum, 1, "", nil}
	} else {
		return newVarFalse()
	}
}

func newVarFromString(str string) Var {
	num, err := strconv.ParseFloat(str, 64)
	if err == nil {
		return Var{VarNum, num, "", nil}
	} else {
		return Var{VarStr, 0, str, nil}
	}
}

func varToInt(v Var) int {
	if v.kind == VarNum {
		return int(v.num)
	} else {
		return 0
	}
}

func varToString(v Var) string {
	switch v.kind {
	case VarFalse:
		return "()"
	case VarNum:
		return strconv.FormatFloat(v.num, 'f', -1, 64)
	case VarStr:
		return v.str
	case VarFunc:
		return "(def function (args ...) ...)"
	default:
		panic("Unknown v.kind")
	}
}

func varCall(v Var, scope *Scope, list []Ast) Var {
	var args []Var
	for _, exp := range list {
		args = append(args, evaluate(exp, scope, true))
	}
	if v.kind == VarFunc {
		return v.fn(args, scope)
	} else {
		panic("Cannot call uncallable object")
	}
}

func (v1 *Var) eq(v2 Var) bool {
	switch v1.kind {
	case VarFalse:
		return v1.kind == v2.kind
	case VarNum:
		if v2.kind == VarNum {
			return math.Abs(v1.num-v2.num) < 0.000001
		} else {
			return false
		}
	case VarStr:
		if v2.kind == VarStr {
			return v1.str == v2.str
		} else {
			return false
		}
	case VarFunc:
		if v2.kind == VarFunc {
			return &v1.fn == &v2.fn
		} else {
			return false
		}
	default:
		panic("Unknown v1.kind")
	}
}

func (v1 *Var) lt(v2 Var) bool {
	switch v1.kind {
	case VarFalse:
		return v2.kind != VarFalse
	case VarNum:
		if v2.kind == VarNum {
			return v1.num < v2.num
		} else {
			return v2.kind != VarFalse
		}
	case VarStr:
		if v2.kind == VarStr {
			return v1.str < v2.str
		} else {
			return v2.kind == VarFunc
		}
	case VarFunc:
		return v2.kind != VarFunc
	default:
		panic("Unknown v1.kind")
	}
}

func (v1 *Var) le(v2 Var) bool {
	return v1.lt(v2) || v1.eq(v2)
}

func (v1 *Var) gt(v2 Var) bool {
	return !v1.le(v2)
}

func (v1 *Var) ge(v2 Var) bool {
	return v1.gt(v2) || v1.eq(v2)
}

func (v1 *Var) add(v2 Var) Var {
	switch v1.kind {
	case VarNum:
		if v2.kind == VarNum {
			return Var{VarNum, v1.num + v2.num, "", nil}
		} else if v2.kind == VarStr {
			str := varToString(*v1)
			return Var{VarStr, 0, str + v2.str, nil}
		}
	case VarStr:
		if v2.kind == VarNum {
			str := varToString(v2)
			return Var{VarNum, 0, v1.str + str, nil}
		} else if v2.kind == VarStr {
			return Var{VarStr, 0, v1.str + v2.str, nil}
		}
	}
	panic("Can only add number or strings!")
}

func (v1 *Var) sub(v2 Var) Var {
	if v1.kind == VarNum && v2.kind == VarNum {
		return Var{VarNum, v1.num - v2.num, "", nil}
	} else {
		panic("Cannot substract non-numerals")
	}
}

func (v1 *Var) mul(v2 Var) Var {
	if v1.kind == VarNum && v2.kind == VarNum {
		return Var{VarNum, v1.num * v2.num, "", nil}
	} else {
		panic("Cannot multiply non-numerals")
	}
}

func (v1 *Var) div(v2 Var) Var {
	if v1.kind == VarNum && v2.kind == VarNum {
		return Var{VarNum, v1.num / v2.num, "", nil}
	} else {
		panic("Cannot divide non-numerals")
	}
}

func (v1 *Var) mod(v2 Var) Var {
	if v1.kind == VarNum && v2.kind == VarNum {
		mod := math.Abs(v2.num)
		ret := v1.num
		for math.Abs(ret) >= mod || ret < 0 {
			if v1.num < 0 {
				ret = ret + mod
			} else {
				ret = ret - mod
			}
		}
		return Var{VarNum, ret, "", nil}
	} else {
		panic("Cannot take the remainder on non-numerals")
	}
}

func evaluate(ast Ast, scope *Scope, evaluateFunction bool) Var {
	switch ast.kind {
	case AstNode:
		return evaluateVar(ast.node, scope)
	case AstList:
		return evaluateList(ast.list, scope, evaluateFunction)
	default:
		panic("Unknown ast.kind")
	}
}

func evaluateVar(str string, scope *Scope) Var {
	v, ok := scopeGet(scope, str)
	if ok {
		return v
	} else {
		return newVarFromString(str)
	}
}

func evaluateList(list []Ast, scope *Scope, evaluateFunction bool) Var {
	if evaluateFunction && len(list) > 0 && list[0].kind == AstNode {
		identifier := list[0].node
		switch identifier {
		case "def":
			return defFnCall(list[1:], scope)
		case "if":
			return newVarFalse() // TODO
		case "loop":
			return newVarFalse() // TODO
		default:
			v, ok := scopeGet(scope, identifier)
			if ok {
				return varCall(v, scope, list[1:])
			}
		}
	}
	ret := newVarFalse()
	for _, exp := range list {
		ret = evaluate(exp, scope, true)
	}
	return ret
}

func defFnCall(args []Ast, scope *Scope) Var {
	if len(args) < 3 {
		panic("'def' should be used as: '(def name (args...) do...)'")
	}
	identifier := varToString(evaluate(args[0], scope, true))
	parameterNames := getParameterNames(args[1], scope)
	ast := Ast{AstList, "", args[2:]}
	rhs := Var{VarFunc, 0, "", func(fnArgs []Var, callScope *Scope) Var {
		fnScope := scopeExtend(scope)
		for i, name := range parameterNames {
			if i < len(fnArgs) {
				scopeSet(&fnScope, name, fnArgs[i])
			} else {
				scopeSet(&fnScope, name, newVarFalse())
			}
		}
		return evaluate(ast, &fnScope, false)
	}}
	scopeSet(scope, identifier, rhs)
	return rhs
}

func getParameterNames(args Ast, scope *Scope) []string {
	var parameterNames []string
	if args.kind == AstNode {
		res := evaluateVar(args.node, scope)
		parameterNames = append(parameterNames, varToString(res))
	} else {
		for _, arg := range args.list {
			res := evaluate(arg, scope, true)
			parameterNames = append(parameterNames, varToString(res))
		}
	}
	return parameterNames
}
