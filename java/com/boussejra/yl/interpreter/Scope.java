package com.boussejra.yl.interpreter;

import com.boussejra.yl.Program;
import com.boussejra.yl.YlException;
import com.boussejra.yl.interpreter.InterpreterException;
import com.boussejra.yl.parser.Ast;
import com.boussejra.yl.parser.AstType;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;


public class Scope {
    private Program program;
    private Scope parent;
    private HashMap<String, Var> vars;

    public Scope() {
        this((Scope) null);
    }

    public Scope(Program program) {
        this();
        this.program = program;
    }

    public Scope(Scope parent) {
        this.parent = parent;
        this.vars = new HashMap<String, Var>();
        if (parent != null) {
            this.program = parent.program;
        } else {
            // Populate global scope
            this.vars.put("print", new Var( args -> {
                for (Var arg: args) {
                    System.out.println(arg);
                }
                return Var.FALSE;
            }));
            this.vars.put("def", new Var( args -> Var.FALSE));
            this.vars.put("let", new Var( args -> Var.FALSE));
            this.vars.put("!", new Var( args -> {
                if (args.size() < 1) {
                    // Seems java does not support exceptions inside lambdas
                    // https://github.com/pivovarit/ThrowingFunction
                    System.err.println("'!' function expects 1 argument");
                    System.exit(1);
                }
                return new Var(args.get(0).getType() == VarType.FALSE);
            }));
            this.vars.put("=", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'=' function expects 2 arguments");
                    System.exit(1);
                }
                return new Var(args.get(0).equals(args.get(1)));
            }));
            this.vars.put(">", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'>' function expects 2 arguments");
                    System.exit(1);
                }
                return new Var(args.get(0).gt(args.get(1)));
            }));
            this.vars.put(">=", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'>=' function expects 2 arguments");
                    System.exit(1);
                }
                return new Var(args.get(0).ge(args.get(1)));
            }));
            this.vars.put("<", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'<' function expects 2 arguments");
                    System.exit(1);
                }
                return new Var(args.get(0).lt(args.get(1)));
            }));
            this.vars.put("<=", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'<=' function expects 2 arguments");
                    System.exit(1);
                }
                return new Var(args.get(0).le(args.get(1)));
            }));
            this.vars.put("+", new Var( args -> {
                Var ret = Var.FALSE;
                try {
                    for (Var arg: args) {
                        ret = ret.add(arg);
                    }
                } catch (InterpreterException e) {
                    System.err.println(e);
                    System.exit(1);
                }
                return ret;
            }));
            this.vars.put("-", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'-' function expects 2 arguments");
                    System.exit(1);
                }
                Var ret = Var.FALSE;
                try {
                    ret = args.get(0).sub(args.get(1));
                } catch (InterpreterException e) {
                    System.err.println(e);
                    System.exit(1);
                }
                return ret;
            }));
            this.vars.put("*", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'*' function expects 2 arguments");
                    System.exit(1);
                }
                Var ret = Var.FALSE;
                try {
                    ret = args.get(0).time(args.get(1));
                } catch (InterpreterException e) {
                    System.err.println(e);
                    System.exit(1);
                }
                return ret;
            }));
            this.vars.put("/", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'/' function expects 2 arguments");
                    System.exit(1);
                }
                Var ret = Var.FALSE;
                try {
                    ret = args.get(0).divide(args.get(1));
                } catch (InterpreterException e) {
                    System.err.println(e);
                    System.exit(1);
                }
                return ret;
            }));
            this.vars.put("%", new Var( args -> {
                if (args.size() < 2) {
                    System.err.println("'%' function expects 2 arguments");
                    System.exit(1);
                }
                Var ret = Var.FALSE;
                try {
                    ret = args.get(0).mod(args.get(1));
                } catch (InterpreterException e) {
                    System.err.println(e);
                    System.exit(1);
                }
                return ret;
            }));
            this.vars.put("argv", new Var( args -> {
                if (args.size() < 1) {
                    System.err.println("'argv' function expects 1 argument");
                    System.exit(1);
                }
                int n = args.get(0).toInt();
                try {
                    ArrayList<Var> ylArgs = this.getYlArgs();
                    if (n < ylArgs.size()) {
                        return ylArgs.get(n);
                    } else {
                        return Var.FALSE;
                    }
                } catch (InterpreterException e) {
                    System.err.println(e);
                    System.exit(1);
                    return Var.FALSE; // Dead code for javac to compile
                }
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

    private Var evaluateList(ArrayList<Ast> list, boolean evaluateFunction) throws YlException {
        if (evaluateFunction && list.size() > 0) {
            if (list.get(0).getType() == AstType.NODE) {
                String identifier = list.get(0).getSym();
                if (identifier.equals("def")) {
                    return this.defFnCall(list.subList(1, list.size()));
                }
                if (identifier.equals("let")) {
                    return this.letFnCall(list.subList(1, list.size()));
                }
                if (identifier.equals("if")) {
                    return this.ifFnCall(list.subList(1, list.size()));
                }
                if (identifier.equals("loop")) {
                    return this.loopFnCall(list.subList(1, list.size()));
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

    private Var defFnCall(List<Ast> list) throws InterpreterException, YlException {
        if (list.size() < 2) {
            throw new InterpreterException("'def' should be used as (def name (args) do something)");
        }
        final Scope scope = this;
        final String identifier = this.evaluate(list.get(0)).toString();
        final ArrayList<String> argNames = this.getArgNames(list.get(1));
        final Ast ast = new Ast(new ArrayList<Ast>(list.subList(2, list.size())));
        Var func = new Var( fnArgs -> {
            Scope fnScope = scope.extend();
            int i = 0;
            for (String name: argNames) {
                if (i < fnArgs.size()) {
                    fnScope.set(name, fnArgs.get(i));
                } else {
                    fnScope.set(name, Var.FALSE);
                }
                i++;
            }
            try {
                return fnScope.evaluate(ast, false);
            } catch (YlException e) {
                // Seems java does not support exceptions inside lambdas
                // https://github.com/pivovarit/ThrowingFunction
                System.err.println(e);
                System.exit(1);
                return Var.FALSE; // Dead code must be added to make the compiler happy
            }
        });
        this.set(identifier, func);
        return func;
    }

    private ArrayList<String> getArgNames(Ast ast) throws YlException {
        ArrayList<String> argNames = new ArrayList<String>();
        if (ast.getType() == AstType.NODE) {
            argNames.add(this.evaluateVar(ast.getSym()).toString());
        } else {
            ArrayList<Ast> list = ast.getList();
            for (Ast exp: list) {
                argNames.add(this.evaluate(exp).toString());
            }
        }
        return argNames;
    }

    private Var letFnCall(List<Ast> list) throws InterpreterException, YlException {
        if (list.size() < 1) {
            throw new InterpreterException("'let' should be used as '(let name value)'");
        }
        String identifier = this.evaluate(list.get(0)).toString();
        Var rhs = list.size() > 1 ? this.evaluate(list.get(1))
                                  : Var.FALSE;
        this.set(identifier, rhs);
        return rhs;
    }

    private Var ifFnCall(List<Ast> list) throws InterpreterException, YlException {
        if (list.size() < 2) {
            throw new InterpreterException("'if' should be used as '(if cond then else)'");
        }
        Var cond = this.evaluate(list.get(0));
        if (cond.toBool()) {
            return this.evaluate(list.get(1));
        } else if (list.size() > 2) {
            return this.evaluate(list.get(2));
        } else {
            return Var.FALSE;
        }
    }

    private Var loopFnCall(List<Ast> list) throws InterpreterException, YlException {
        if (list.size() < 3) {
            throw new InterpreterException("'loop' should be used as '(loop identifier (list...) (do something))'");
        }
        String identifier = this.evaluate(list.get(0)).toString();
        ArrayList<Var> loopValues = this.getLoopList(list.get(1));
        Var ret = Var.FALSE;
        for (Var var: loopValues) {
            Scope loopScope = this.extend();
            loopScope.set(identifier, var);
            ret = loopScope.evaluate(list.get(2));
        }
        return ret;
    }

    private ArrayList<Var> getLoopList(Ast ast) throws YlException {
        ArrayList<Var> list = new ArrayList<Var>();
        if (ast.getType() == AstType.NODE) {
            list.add(this.evaluateVar(ast.getSym()));
        } else {
            ArrayList<Ast> exps = ast.getList();
            if (exps.get(0).getSym().equals("range")) {
                double min, max;
                if (exps.size() < 3) {
                    min = 0;
                    max = this.evaluate(exps.get(1)).toDouble();
                } else {
                    min = this.evaluate(exps.get(1)).toDouble();
                    max = this.evaluate(exps.get(2)).toDouble();
                }
                while (min < max) {
                    list.add(new Var(min));
                    min += 1;
                }
            } else {
                for (Ast exp: exps) {
                    list.add(this.evaluate(exp));
                }
            }

        }
        return list;
    }

    private ArrayList<Var> getYlArgs() throws InterpreterException {
        if (this.program != null) {
            return this.program.getArgs();
        } else {
            throw new InterpreterException("Seems like YlArgs were not set! Are you in interactive mode?");
        }
    }
}
