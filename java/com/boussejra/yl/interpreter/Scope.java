package com.boussejra.yl.interpreter;

import com.boussejra.yl.YlException;
import com.boussejra.yl.parser.Ast;
import com.boussejra.yl.parser.AstType;

import java.util.ArrayList;
import java.util.HashMap;


public class Scope {
    private Scope parent;
    private HashMap<String, Var> vars;

    public Scope() {
        this(null);
    }

    public Scope(Scope parent) {
        this.parent = parent;
        this.vars = new HashMap<String, Var>();
        if (parent == null) {
            this.vars.put("print", new Var( args -> {
                for (Var arg: args) {
                    System.out.println(arg);
                }
                return Var.FALSE;
            }));
        }
    }

    public Scope extend() {
        return new Scope(this);
    }

    public Var get(String name) {
        Var var = this.vars.get(name);
        if (var != null) {
            return var;
        } else if (this.parent != null) {
            return parent.get(name);
        } else {
            return null;
        }
    }

    public void set(String name, Var var) {
        this.vars.put(name, var);
    }

    public Var evaluate(Ast ast) throws YlException {
        return this.evaluate(ast, true);
    }

    public Var evaluate(Ast ast, boolean evaluateFunction) throws YlException {
        switch (ast.getType()) {
        case NODE:
            return this.evaluateVar(ast.getSym());
        case LIST:
            return this.evaluateList(ast.getList(), evaluateFunction);
        default:
            throw new YlException("Unknown AstType");
        }
    }

    private Var evaluateVar(String str) {
        Var var = this.get(str);
        if (var != null) {
            return var;
        } else {
            return Var.fromString(str);
        }
    }

    private Var evaluateList(ArrayList<Ast> list, boolean evaluateFunction)  throws YlException {
        if (evaluateFunction && list.size() > 0) {
            if (list.get(0).getType() == AstType.NODE) {
                String identifier = list.get(0).getSym();
                if (identifier.equals("def")) {
                    // TODO
                }
                if (identifier.equals("let")) {
                    // TODO
                }
                if (identifier.equals("if")) {
                    // TODO
                }
                if (identifier.equals("loop")) {
                    // TODO
                }
                Var var = this.get(identifier);
                if (var != null)
                    return var.call(this, list.subList(1, list.size()));
            }
        }
        Var ret = Var.FALSE;
        for (Ast expr: list) {
            ret = this.evaluate(expr);
        }
        return ret;
    }

}
