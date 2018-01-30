package main

import (
	"fmt"
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
	return Var{VarStr, 0, str, nil}
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
			return newVarFalse() // TODO
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
