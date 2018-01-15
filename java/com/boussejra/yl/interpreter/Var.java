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

    public boolean equals(Var var) {
        switch (this.type) {
        case FALSE:
            return var.type == VarType.FALSE;
        case NUMBER:
            if (var.type == VarType.NUMBER)
                return Math.abs(this.num - var.num) < 0.000001;
            else
                return false;
        case STRING:
            if (var.type == VarType.STRING)
                return this.str.equals(var.str);
            else
                return false;
        case FUNCTION:
            if (var.type == VarType.FUNCTION)
                return this.func == var.func;
            else
                return false;
        default:
            return false; // Unreachable... Avoid javac error
        }
    }

    public boolean lt(Var var) {
        switch (this.type) {
        case FALSE:
            return var.type != VarType.FALSE;
        case NUMBER:
            if (var.type == VarType.NUMBER)
                return this.num < var.num;
            else
                return var.type != VarType.FALSE;
        case STRING:
            if (var.type == VarType.STRING)
                return this.str.compareTo(var.str) < 0;
            else
                return var.type == VarType.FUNCTION;
        case FUNCTION:
            return var.type != VarType.FUNCTION;
        default:
            return false; // Unreachable... Avoid javac error
        }
    }

    public boolean le(Var var) {
        return this.lt(var) || this.equals(var);
    }

    public boolean gt(Var var) {
        return !this.le(var);
    }

    public boolean ge(Var var) {
        return this.gt(var) || this.equals(var);
    }

    public Var add(Var var) throws InterpreterException {
        switch (this.type) {
        case FALSE:
            return var;
        case NUMBER:
            if (var.type == VarType.NUMBER) {
                return new Var(this.num + var.num);
            } else if (var.type == VarType.STRING) {
                Var var1s = new Var(this.toString());
                return new Var(var1s.str + var.str);
            }
            break;
        case STRING:
            if (var.type == VarType.NUMBER) {
                Var var2s = new Var(var.toString());
                return new Var(this.str + var2s.str);
            } else if (var.type == VarType.STRING) {
                return new Var(this.str + var.str);
            }
        }
        throw new InterpreterException("Cannot only add number or strings");
    }

    public Var sub(Var var) throws InterpreterException {
        if (this.type == VarType.NUMBER && var.type == VarType.NUMBER) {
            return new Var(this.num - var.num);
        } else {
            throw new InterpreterException("Cannot substract non-numerals");
        }
    }

    public Var time(Var var) throws InterpreterException {
        if (this.type == VarType.NUMBER && var.type == VarType.NUMBER) {
            return new Var(this.num * var.num);
        } else {
            throw new InterpreterException("Cannot multiply non-numerals");
        }
    }

    public Var divide(Var var) throws InterpreterException {
        if (this.type == VarType.NUMBER && var.type == VarType.NUMBER) {
            return new Var(this.num / var.num);
        } else {
            throw new InterpreterException("Cannot divide non-numerals");
        }
    }

    public Var mod(Var var) throws InterpreterException {
        if (this.type == VarType.NUMBER && var.type == VarType.NUMBER) {
            return new Var(this.num % var.num);
        } else {
            throw new InterpreterException("Cannot take the remainder on non-numerals");
        }
    }
}
