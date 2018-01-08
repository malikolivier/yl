use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::rc::Weak;
use std::process;
use std::str::FromStr;

use parser::AstNode;


fn croak(msg: &str) {
    eprintln!("{}", msg);
    process::exit(1)
}

#[derive(Debug, Clone)]
pub struct UserDefinedFunc {
    scope: Weak<Scope>,
    ast: Weak<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncType {
    LetFn,
    PrintFn,
    NotOp,
    EqOp,
    UserDefined(UserDefinedFunc),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    kind: FuncType,
    args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
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

impl PartialEq for UserDefinedFunc {
    fn eq(&self, other: &UserDefinedFunc) -> bool {
        true // TODO
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
}

impl ScopeContainer {
    fn extend(&self) -> ScopeContainer {
        ScopeContainer {
            scope: Rc::new(Scope {
                parent: Some(self.scope.clone()),
                vars: RefCell::new(HashMap::<String, Var>::new()),
            })
        }
    }

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
            FuncType::PrintFn => {
                FuncType::print_fn(&self.get_args(ast))
            },
            FuncType::NotOp => {
                FuncType::not_op(&self.get_args(ast))
            },
            FuncType::EqOp => {
                FuncType::eq_op(&self.get_args(ast))
            },
            FuncType::UserDefined(ref func) => {
                // TODO
                croak("UserDefined functions not implemented");
                Var::False
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

    fn print_fn(args: &[Var]) -> Var {
        let refs = args.into_iter().map(|var| var).collect();
        print_fn(refs);
        Var::False
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
}
