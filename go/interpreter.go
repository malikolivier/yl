package main

import (
	"strconv"
)

type Scope struct {
	parent *Scope
	vars   map[string]Var
}

func createParentScope() Scope {
	var vars map[string]Var
	return Scope{nil, vars}
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

const (
	VarFalse = iota
	VarNum
	VarStr
	VarFunc
)

type Var struct {
	kind int
	num float64
	str string
	fn func([]Var, *Scope)Var
}

func newVarFalse() Var {
	return Var{VarFalse, 0, "", nil}
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

func evaluate(ast Ast, scope *Scope, evaluateFunction bool) Var {
	switch (ast.kind) {
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
	// TODO
	return newVarFalse()
}
