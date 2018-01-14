package com.boussejra.yl.interpreter;

import java.util.HashMap;


public class Scope {
    private Scope parent;
    private HashMap<String, Var> vars;

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
}
