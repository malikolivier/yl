package com.boussejra.yl;

import com.boussejra.yl.YlException;
import com.boussejra.yl.interpreter.Scope;
import com.boussejra.yl.interpreter.Var;
import com.boussejra.yl.parser.Ast;
import com.boussejra.yl.parser.AstType;
import com.boussejra.yl.parser.ParseException;

import java.util.ArrayList;
import java.util.stream.Collectors;


public class Program {
    private Ast ast;
    private ArrayList<Var> args;

    public Program(String code, String[] args) throws ParseException {
        this.ast = Ast.parseCode(code);
        this.args = new ArrayList<Var>();
        for (String arg: args) {
            this.args.add(Var.fromString(arg));
        }
    }

    public Ast getAst() {
        return this.ast;
    }

    public ArrayList<Var> getArgs() {
        return this.args;
    }

    public Var run() throws YlException {
        return new Scope(this).evaluate(this.ast, false);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("ARGS: ");
        sb.append(
            this.args.stream().map(Var::toString).collect(Collectors.joining(", "))
        );
        sb.append('\n');
        sb.append(ast.toString());
        return sb.toString();
    }
}
