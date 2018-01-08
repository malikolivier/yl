use std::env;
use std::f64;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::process;
use std::str::FromStr;
use std::ops::Add;
use std::ops::Sub;
use std::ops::Mul;
use std::ops::Div;
use std::ops::Rem;

use parser::AstNode;


fn croak(msg: &str) {
    eprintln!("{}", msg);
    process::exit(1)
}

#[derive(Debug, Clone)]
pub struct UserDefinedFunc {
    scope: Rc<Scope>,
    ast: AstNode,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum FuncType {
    LetFn,
    DefFn,
    IfFn,
    LoopFn,
    PrintFn,
    ArgvFn,
    NotOp,
    EqOp,
    GtOp,
    GeOp,
    LtOp,
    LeOp,
    PlusOp,
    MinusOp,
    MultiplyOp,
    DivideOp,
    ModuloOp,
    UserDefined(UserDefinedFunc),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Func {
    kind: FuncType,
    args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Var {
    False,
    Num(f64),
    Str(String),
    Func(Func)
}

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<Rc<Scope>>,
    vars: RefCell<HashMap<String, Var>>,
}

pub struct ScopeContainer {
    scope: Rc<Scope>,
}

impl Var {
    fn to_string(&self) -> String {
        match self {
            &Var::False => "".to_string(),
            &Var::Num(n) => n.to_string(),
            &Var::Str(ref s) => s.clone(),
            &Var::Func(_) => "(def function (args ...) ...)".to_string(), // TODO
        }
    }

    fn get_true() -> Var {
        Var::Num(1 as f64)
    }
}

impl<'a> From<&'a Var> for f64 {
    fn from(var: &Var) -> Self {
        match var {
            &Var::Num(n) => n,
            _ => 0.,
        }
    }
}

impl PartialEq for UserDefinedFunc {
    fn eq(&self, other: &UserDefinedFunc) -> bool {
        self.ast.eq(&other.ast) /* TODO: Take Scope into account? */
    }
}

impl PartialOrd for UserDefinedFunc {
     fn partial_cmp(&self, _other: &UserDefinedFunc) -> Option<Ordering> {
         /* No way to compare user defined functions! */
         None
     }
}

impl<'a> Add<&'a Var> for &'a Var {
    type Output = Var;
    fn add(self, other: &Var) -> Var {
        match (self, other) {
            (&Var::False, &Var::False) => Var::False,
            (&Var::False, &Var::Num(n)) => Var::Num(n),
            (&Var::Num(n), &Var::False) => Var::Num(n),
            (&Var::Num(n1), &Var::Num(n2)) => Var::Num(n1 + n2),
            _ => {
                let mut ret = self.to_string();
                ret.push_str(&other.to_string());
                Var::Str(ret)
            },
        }
    }
}

impl<'a> Sub<&'a Var> for &'a Var {
    type Output = Var;
    fn sub(self, other: &Var) -> Var {
        match (self, other) {
            (&Var::False, &Var::False) => Var::Num(0 as f64),
            (&Var::False, &Var::Num(n)) => Var::Num(-n),
            (&Var::Num(n), &Var::False) => Var::Num(n),
            (&Var::Num(n1), &Var::Num(n2)) => Var::Num(n1 - n2),
            _ => {
                croak("Cannot substract non-numbers!");
                unreachable!()
            },
        }
    }
}

impl<'a> Mul<&'a Var> for &'a Var {
    type Output = Var;
    fn mul(self, other: &Var) -> Var {
        match (self, other) {
            (&Var::False, _) => Var::Num(0 as f64),
            (_, &Var::False) => Var::Num(0 as f64),
            (&Var::Num(n1), &Var::Num(n2)) => Var::Num(n1 * n2),
            _ => {
                croak("Cannot multiply non-numbers!");
                unreachable!()
            },
        }
    }
}

impl<'a> Div<&'a Var> for &'a Var {
    type Output = Var;
    fn div(self, other: &Var) -> Var {
        match (self, other) {
            (&Var::False, _) => Var::Num(0.),
            (&Var::Num(n), &Var::False) => if n > 0. {
                                               Var::Num(f64::INFINITY)
                                           } else if n < 0. {
                                               Var::Num(f64::NEG_INFINITY)
                                           } else {
                                               Var::Num(f64::NAN)
                                           }
            (&Var::Num(n1), &Var::Num(n2)) => Var::Num(n1 / n2),
            _ => {
                croak("Cannot divide non-numbers!");
                unreachable!()
            },
        }
    }
}

impl<'a> Rem<&'a Var> for &'a Var {
    type Output = Var;
    fn rem(self, other: &Var) -> Var {
        match (self, other) {
            (&Var::False, _) => Var::Num(0.),
            (&Var::Num(n), &Var::Num(modulo)) => Var::Num(n % modulo),
            _ => {
                croak("Cannot do modulo on non-numbers!");
                unreachable!()
            },
        }
    }
}


trait ToVar {
    fn to_var(&self) -> Var;
}

impl ToVar for bool {
    fn to_var(&self) -> Var {
        match self {
            &false => Var::False,
            &true => Var::get_true(),
        }
    }
}

impl Func {
    fn call(&self, args: &[Var]) -> Var {
        /* Assumes self is of type UserDefined */
        let func = self.unwrap();
        let fn_scope_container = Scope::extend(&func.scope);
        for (i, arg_name) in self.args.iter().enumerate() {
            if i < args.len() {
                fn_scope_container.scope.set(arg_name, args[i].clone());
            } else {
                fn_scope_container.scope.set(arg_name, Var::False);
            }
        }
        fn_scope_container.evaluate(&func.ast, false)
    }

    fn unwrap(&self) -> &UserDefinedFunc {
        if let FuncType::UserDefined(ref func) = self.kind {
            func
        } else {
            unreachable!()
        }
    }
}

impl Scope {
    pub fn global() -> ScopeContainer {
        let mut vars = HashMap::<String, Var>::new();
        vars.insert("let".to_string(), Var::Func(Func {
            kind: FuncType::LetFn,
            args: vec!["id".to_string(), "rhs".to_string()],
        }));
        vars.insert("print".to_string(), Var::Func(Func {
            kind: FuncType::PrintFn,
            args: vec!["var".to_string()],
        }));
        vars.insert("!".to_string(), Var::Func(Func {
            kind: FuncType::NotOp,
            args: vec!["var".to_string()],
        }));
        vars.insert("=".to_string(), Var::Func(Func {
            kind: FuncType::EqOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert(">".to_string(), Var::Func(Func {
            kind: FuncType::GtOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert(">=".to_string(), Var::Func(Func {
            kind: FuncType::GeOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert("<".to_string(), Var::Func(Func {
            kind: FuncType::LtOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert("<=".to_string(), Var::Func(Func {
            kind: FuncType::LeOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert("+".to_string(), Var::Func(Func {
            kind: FuncType::PlusOp,
            args: vec!["var1".to_string(), "var2".to_string(), "...".to_string()],
        }));
        vars.insert("-".to_string(), Var::Func(Func {
            kind: FuncType::MinusOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert("*".to_string(), Var::Func(Func {
            kind: FuncType::MultiplyOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert("/".to_string(), Var::Func(Func {
            kind: FuncType::DivideOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert("%".to_string(), Var::Func(Func {
            kind: FuncType::ModuloOp,
            args: vec!["var1".to_string(), "var2".to_string()],
        }));
        vars.insert("def".to_string(), Var::Func(Func {
            kind: FuncType::DefFn,
            args: vec!["id".to_string(), "(args...)".to_string(), "...".to_string()],
        }));
        vars.insert("if".to_string(), Var::Func(Func {
            kind: FuncType::IfFn,
            args: vec!["condition".to_string(), "(exp1...)".to_string(), "(exp2...)".to_string()],
        }));
        vars.insert("loop".to_string(), Var::Func(Func {
            kind: FuncType::LoopFn,
            args: vec!["identifier".to_string(), "(list ...)".to_string(), "(exp ...)".to_string()],
        }));
        vars.insert("argv".to_string(), Var::Func(Func {
            kind: FuncType::ArgvFn,
            args: vec!["n".to_string()],
        }));
        ScopeContainer {
            scope: Rc::new(Scope {
                parent: None,
                vars: RefCell::new(vars),
            })
        }
    }

    fn get(&self, name: &str) -> Option<Var> {
        match self.vars.borrow().get(name) {
            None => match self.parent {
                None => None,
                Some(ref p) => p.get(name),
            },
            Some(v) => Some(v.clone()),
        }
    }

    fn set(&self, name: &str, value: Var) {
        self.vars.borrow_mut().insert(name.to_string(), value);
    }

    fn extend(parent: &Rc<Scope>) -> ScopeContainer {
        ScopeContainer {
            scope: Rc::new(Scope {
                parent: Some(parent.clone()),
                vars: RefCell::new(HashMap::<String, Var>::new()),
            })
        }
    }
}

impl ScopeContainer {
    pub fn evaluate(&self, ast: &AstNode, evaluate_function: bool) -> Var {
        match ast {
            &AstNode::Val(ref string) => self.evaluate_val(string),
            &AstNode::List(ref vec) => self.evaluate_list(&vec, evaluate_function),
        }
    }

    fn evaluate_val(&self, string: &str) -> Var {
        let ret = self.scope.get(string);
        match ret {
            None => parse_to_yl_var(string),
            Some(var) => var.clone(),
        }
    }

    fn evaluate_list(&self, vec: &[AstNode], evaluate_function: bool) -> Var {
        if evaluate_function && vec.len() > 0 {
            let mut call_func = false;
            match vec[0] {
                AstNode::List(_) => {},
                AstNode::Val(ref fn_name) =>
                    match self.scope.get(&fn_name) {
                        None => {},
                        Some(_func) => {
                            call_func = true;
                        },
                    },
            }
            if call_func {
                return self.call(vec);
            }
        }
        self.evaluate_list_fallback(vec)
    }

    fn evaluate_list_fallback(&self, vec: &[AstNode]) -> Var {
        if vec.len() <= 0 {
            return Var::False;
        }
        let mut i = 0;
        while i < vec.len() - 1 {
            self.evaluate(&vec[i], true);
            i += 1;
        }
        self.evaluate(&vec[i], true)
    }

    fn call(&self, ast: &[AstNode]) -> Var {
        let f = self.check_and_return_function(ast);
        match f.kind {
            FuncType::LetFn => {
                let args = self.get_args(ast);
                FuncType::let_fn(&args, self)
            },
            FuncType::DefFn => {
                if ast.len() < 2 {
                    croak("To define a function, do: '(def identifier (arg1 arg2 ...) do_something)'");
                    unreachable!();
                }
                let identifier = self.get_args(&ast[..2])[0].to_string();
                FuncType::def_fn(identifier, &ast[2..], self)
            },
            FuncType::IfFn => {
                if ast.len() < 2 {
                    croak("To use the 'if' function, do: '(if cond (exp1 ) (exp2 ))'");
                    unreachable!();
                }
                let condition = &self.get_args(&ast[..2])[0];
                FuncType::if_fn(condition, &ast[2..], self)
            },
            FuncType::LoopFn => {
                if ast.len() < 2 {
                    croak("To use the 'loop' function, do: '(loop id (list ...) (loop exp ))'");
                    unreachable!();
                }
                let identifier = self.get_args(&ast[..2])[0].to_string();
                FuncType::loop_fn(identifier, &ast[2..], self)
            },
            FuncType::PrintFn => {
                FuncType::print_fn(&self.get_args(ast))
            },
            FuncType::ArgvFn => {
                FuncType::argv_fn(&self.get_args(ast))
            },
            FuncType::NotOp => {
                FuncType::not_op(&self.get_args(ast))
            },
            FuncType::EqOp => {
                FuncType::eq_op(&self.get_args(ast))
            },
            FuncType::GtOp => {
                FuncType::gt_op(&self.get_args(ast))
            },
            FuncType::GeOp => {
                FuncType::ge_op(&self.get_args(ast))
            },
            FuncType::LtOp => {
                FuncType::lt_op(&self.get_args(ast))
            },
            FuncType::LeOp => {
                FuncType::le_op(&self.get_args(ast))
            },
            FuncType::PlusOp => {
                FuncType::plus_op(&self.get_args(ast))
            },
            FuncType::MinusOp => {
                FuncType::minus_op(&self.get_args(ast))
            },
            FuncType::MultiplyOp => {
                FuncType::multiply_op(&self.get_args(ast))
            },
            FuncType::DivideOp => {
                FuncType::divide_op(&self.get_args(ast))
            },
            FuncType::ModuloOp => {
                FuncType::modulo_op(&self.get_args(ast))
            },
            FuncType::UserDefined(_) => {
                let args = self.get_args(ast);
                f.call(&args)
            }
        }
    }

    fn check_and_return_function(&self, ast: &[AstNode]) -> Func {
        match ast[0] {
            AstNode::List(_) => unreachable!(),
            AstNode::Val(ref fn_name) =>
                match self.scope.get(&fn_name) {
                    None => unreachable!(),
                    Some(var) => {
                        match var {
                            Var::Func(f) => f,
                            _ => {
                                croak("Not callable!");
                                unreachable!()
                            }
                        }
                    },
                },
        }
    }

    fn get_args(&self, ast: &[AstNode]) -> Vec<Var>{
        let mut args = Vec::<Var>::new();
        let mut i = 0;
        while i < ast.len() {
            if i == 0 {
                i += 1;
                continue
            }
            args.push(self.evaluate(&ast[i], true));
            i += 1;
        }
        args
    }
}

fn parse_to_yl_var(string: &str) -> Var {
    match f64::from_str(string) {
        Err(_) => {
            let s = string.to_string();
            Var::Str(s)
        },
        Ok(n) => Var::Num(n),
    }
}

fn print_fn(argv: Vec<&Var>) {
    for var in argv.iter() {
        match *var {
            &Var::False => println!("()"),
            &Var::Num(n) => println!("{}", n),
            &Var::Str(ref s) => println!("{}", s),
            &Var::Func(ref f) => print_function(f),
        }
    }
}

fn print_function(f: &Func) {
    print!("(def function (");
    for arg in f.args.clone() {
        print!("{} ", arg);
    }
    print!(")");
    let kind = &f.kind;
    match kind {
        &FuncType::UserDefined(ref _f) => {
            print!(" ... "); // TODO
        },
        _ => print!(" [native code] "),
    }
    println!(")");
}


pub fn print(var: &Var) {
    let mut vec = Vec::<&Var>::new();
    vec.push(var);
    print_fn(vec)
}

struct MinMax {
    min: f64,
    max: f64,
}

enum LoopValues<'a> {
    ValueList(&'a AstNode),
    MinMax(MinMax)
}

impl<'a> LoopValues<'a> {
    fn new(values: &'a AstNode, scope_container: &ScopeContainer) -> LoopValues<'a> {
        if let &AstNode::List(ref nodes) = values {
            if nodes.len() > 1 {
                let first_node = &nodes[0];
                if let &AstNode::Val(ref val) = first_node {
                    if val == "range" {
                        let second_node = &nodes[1];
                        let (min, max) = if nodes.len() > 2 {
                            let third_node = &nodes[2];
                            (f64::from(&scope_container.evaluate(second_node, true)),
                             f64::from(&scope_container.evaluate(third_node, true)))
                        } else {
                            (0.,
                             f64::from(&scope_container.evaluate(second_node, true)))
                        };
                        return LoopValues::MinMax(MinMax { min, max })
                    }
                }
            }
        }
        LoopValues::ValueList(values)

    }

    fn iter(self, scope_container: &'a ScopeContainer) -> LoopIterator<'a> {
        LoopIterator {
            values: self,
            cur: 0,
            scope_container,
        }
    }
}

struct LoopIterator<'a> {
    values: LoopValues<'a>,
    cur: usize,
    scope_container: &'a ScopeContainer
}

impl<'a> Iterator for LoopIterator<'a> {
    type Item = Var;
    fn next(&mut self) -> Option<Var> {
        let ret = match self.values {
            LoopValues::ValueList(ast) =>
                match ast {
                    &AstNode::Val(_) => {
                        if self.cur > 0 {
                            None
                        } else {
                            Some(self.scope_container.evaluate(ast, true))
                        }
                    },
                    &AstNode::List(ref node_list) => {
                        if self.cur < node_list.len() {
                            Some(self.scope_container.evaluate(&node_list[self.cur], true))
                        } else {
                            None
                        }
                    }
                },
            LoopValues::MinMax(ref minmax) => {
                let next = minmax.min + (self.cur as f64);
                if next < minmax.max {
                    Some(Var::Num(next))
                } else {
                    None
                }
            }
        };
        self.cur += 1;
        ret
    }
}

impl FuncType {
    fn let_fn(args: &[Var], scope_container: &ScopeContainer) -> Var {
        if args.len() < 1 {
            croak("'let' function requires at least 1 argument!");
            unreachable!()
        }
        let identifier = args[0].to_string();
        let rhs = if args.len() > 1 {
            args[1].clone()
        } else {
            Var::False
        };
        let ret = rhs.clone();
        scope_container.scope.set(&identifier, rhs);
        ret
    }

    fn def_fn(identifier: String, args: &[AstNode], scope_container: &ScopeContainer) -> Var {
        let mut fn_args = Vec::<String>::new();
        if args.len() > 0 {
            match args[0] {
                AstNode::Val(ref string) => fn_args.push(string.to_string()),
                AstNode::List(ref arg_names) => {
                    for name in arg_names {
                        match name {
                            &AstNode::Val(ref string) => fn_args.push(string.to_string()),
                            &AstNode::List(_) => fn_args.push(
                                scope_container.evaluate(&name, true).to_string()
                            ),
                        }
                    }
                },
            }
        }
        let rhs = Var::Func(Func {
            kind: FuncType::UserDefined(UserDefinedFunc {
                scope: scope_container.scope.clone(),
                ast: AstNode::List(if args.len() > 1 {
                                       args[1..].to_vec()
                                   } else {
                                       Vec::<AstNode>::new()
                                   }),
            }),
            args: fn_args
        });
        let ret = rhs.clone();
        scope_container.scope.set(&identifier, rhs);
        ret
    }

    fn if_fn(condition: &Var, exps: &[AstNode], scope_container: &ScopeContainer) -> Var {
        match condition {
            &Var::False => if exps.len() > 1 {
                               scope_container.evaluate(&exps[1], false)
                           } else {
                               Var::False
                           },
            _ => if exps.len() > 0 {
                    scope_container.evaluate(&exps[0], false)
                 } else {
                     Var::False
                 },
        }
    }

    fn loop_fn(identifier: String, exps: &[AstNode], scope_container: &ScopeContainer) -> Var {
        let loop_scope_container = Scope::extend(&scope_container.scope);
        let mut ret = Var::False;
        if exps.len() < 1 {
            return ret;
        }
        for value in LoopValues::new(&exps[0], scope_container).iter(scope_container) {
            loop_scope_container.scope.set(&identifier, value);
            ret = loop_scope_container.evaluate(&AstNode::List(exps[1..].to_vec()), true);
        }
        ret
    }

    fn print_fn(args: &[Var]) -> Var {
        let refs = args.into_iter().map(|var| var).collect();
        print_fn(refs);
        Var::False
    }

    fn argv_fn(args: &[Var]) -> Var {
        if args.len() < 1 {
            croak("'argv' function requires 1 argument!");
            unreachable!()
        }
        let n = 2 + (f64::from(&args[0]) as usize);
        let app_args: Vec<String> = env::args().collect();
        if n < app_args.len() {
            parse_to_yl_var(&app_args[n])
        } else {
            Var::False
        }
    }

    fn not_op(args: &[Var]) -> Var {
        if args.len() < 1 {
            Var::get_true()
        } else {
            match args[0] {
                Var::False => true,
                _ => false,
            }.to_var()
        }
    }

    fn eq_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'=' function requires 2 arguments!");
            unreachable!()
        }
        args[0].eq(&args[1]).to_var()
    }

    fn gt_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'>' function requires 2 arguments!");
            unreachable!()
        }
        args[0].gt(&args[1]).to_var()
    }

    fn ge_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'>=' function requires 2 arguments!");
            unreachable!()
        }
        args[0].ge(&args[1]).to_var()
    }

    fn lt_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'<' function requires 2 arguments!");
            unreachable!()
        }
        args[0].lt(&args[1]).to_var()
    }

    fn le_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'<=' function requires 2 arguments!");
            unreachable!()
        }
        args[0].le(&args[1]).to_var()
    }

    fn plus_op(args: &[Var]) -> Var {
        let mut out = Var::False;
        for var in args {
            out = &out + &var;
        }
        out
    }

    fn minus_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'-' function requires 2 arguments!");
            unreachable!()
        }
        &args[0] - &args[1]
    }

    fn multiply_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'-' function requires 2 arguments!");
            unreachable!()
        }
        &args[0] * &args[1]
    }

    fn divide_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'-' function requires 2 arguments!");
            unreachable!()
        }
        &args[0] / &args[1]
    }

    fn modulo_op(args: &[Var]) -> Var {
        if args.len() != 2 {
            croak("'-' function requires 2 arguments!");
            unreachable!()
        }
        &args[0] % &args[1]
    }
}
