package com.boussejra.yl.interpreter;

import com.boussejra.yl.interpreter.VarType;

public class Var {
    private VarType type;
    private double num;
    private String str;
    public static final Var FALSE = new Var();

    public Var() {
        this.type = VarType.FALSE;
    }

    public Var(String string) {
        this.type = VarType.STRING;
        this.str = string;
    }

    public int toInt() {
        if (this.type == VarType.NUMBER) {
            return (int) this.num;
        } else {
            return 0;
        }
    }

    public static Var fromString(String string) {
        return new Var(string);
    }

    @Override
    public String toString() {
        switch (this.type) {
        case FALSE:
            return "()";
        case NUMBER:
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
