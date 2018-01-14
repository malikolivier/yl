package com.boussejra.yl.interpreter;

import com.boussejra.yl.YlException;
import com.boussejra.yl.interpreter.InterpreterException;
import com.boussejra.yl.interpreter.VarType;
import com.boussejra.yl.parser.Ast;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;


public class Var {
    private VarType type;
    private double num;
    private String str;
    private Function<ArrayList<Var>, Var> func;

    public static final Var FALSE = new Var();
    public static final Var TRUE = new Var(1.);

    public Var() {
        this.type = VarType.FALSE;
    }

    public Var(Double d) {
        this.type = VarType.NUMBER;
        this.num = d;
    }

    public Var(boolean b) {
        if (b) {
            this.type = VarType.NUMBER;
            this.num = 1;
        } else {
            this.type = VarType.FALSE;
        }
    }

    public Var(String string) {
        this.type = VarType.STRING;
        this.str = string;
    }

    public Var(Function<ArrayList<Var>, Var> func) {
        this.type = VarType.FUNCTION;
        this.func = func;
    }

    public VarType getType() {
        return this.type;
    }

    public int toInt() {
        if (this.type == VarType.NUMBER) {
            return (int) this.num;
        } else {
            return 0;
        }
    }

    public boolean toBool() {
        return this.type != VarType.FALSE;
    }

    public static Var fromString(String string) {
        try {
            return new Var(Double.parseDouble(string));
        } catch (NumberFormatException e) {
            return new Var(string);
        }
    }

    public Var call(Scope scope, List<Ast> args) throws YlException {
        ArrayList<Var> fnArgs = new ArrayList<Var>();
        for (Ast arg: args) {
            fnArgs.add(scope.evaluate(arg));
        }
        switch (this.type) {
        case FUNCTION:
            return this.func.apply(fnArgs);
        default:
            throw new InterpreterException("Cannot call uncallable object!");
        }
    }

    @Override
    public String toString() {
        switch (this.type) {
        case FALSE:
            return "()";
        case NUMBER:
            double floored = Math.floor(this.num);
            if (Math.abs(this.num - Math.floor(this.num)) < 0.000001)
                return Integer.toString((int) floored);
            else
                return Double.toString(this.num);
        case STRING:
            return this.str;
        case FUNCTION:
            return "(def function (args...) do...)";
        default:
            return ""; // Unreachable... Avoid javac error
        }
    }
}
