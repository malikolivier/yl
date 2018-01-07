use std::collections::HashMap;
use std::rc::Rc;
use std::process;
use std::str::FromStr;

use parser::AstNode;


fn croak(msg: &str) {
    eprintln!("{}", msg);
    process::exit(1)
}

#[derive(Debug, Clone)]
pub struct UserDefinedFunc<'s> {
    scope: Rc<Scope<'s>>,
    ast: Rc<AstNode>,
}

#[derive(Debug, Clone)]
pub enum FuncType<'s> {
    LetFn,
    UserDefined(UserDefinedFunc<'s>),
}

#[derive(Debug, Clone)]
pub struct Func<'s> {
    kind: FuncType<'s>,
    args: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Var<'s> {
    False,
    Num(f64),
    Str(String),
    Func(Func<'s>)
}

#[derive(Debug, Clone)]
pub struct Scope<'s> {
    parent: Option<&'s Scope<'s>>,
    vars: HashMap<String, Var<'s>>,
}

#[derive(Debug, Clone)]
struct CallContext<'s> {
    scope: &'s Scope<'s>,
    func: &'s Func<'s>,
}

impl<'s> CallContext<'s> {
    fn new<'a>(scope: &'a mut Scope, vec: &Vec<AstNode>) -> CallContext<'a> {
        match vec[0] {
            AstNode::List(_) => unreachable!(),
            AstNode::Val(ref fn_name) =>
                match scope.get(&fn_name) {
                    None => unreachable!(),
                    Some(func) => {
                        match func {
                            &Var::Func(ref f) => CallContext { scope, func: f },
                            _ => {
                                croak("Not callable");
                                unreachable!()
                            }
                        }
                    },
                },
        }
    }
}

impl<'s> Scope<'s> {
    pub fn global() -> Scope<'s> {
        let mut vars = HashMap::<String, Var>::new();
        vars.insert("let".to_string(), Var::Func(Func {
            kind: FuncType::LetFn,
            args: vec!["id".to_string(), "rhs".to_string()],
        }));
        Scope {
            parent: None,
            vars,
        }
    }

    fn extend(&self) -> Scope {
        Scope {
            parent: Some(self),
            vars: HashMap::<String, Var>::new(),
        }
    }

    fn get(&'s self, name: &str) -> Option<&'s Var> {
        match self.vars.get(name) {
            None => match self.parent {
                None => None,
                Some(p) => p.get(name),
            },
            Some(v) => Some(v),
        }
    }

    fn set(&'s mut self, name: &str, value: Var<'s>) {
        self.vars.insert(name.to_string(), value);
    }

    pub fn evaluate<'a>(&'a mut self, ast: &AstNode, evaluate_function: bool) -> Var<'a> {
        match ast {
            &AstNode::Val(ref string) => self.evaluate_val(string),
            &AstNode::List(ref vec) => self.evaluate_list(&vec, evaluate_function),
        }
    }

    fn evaluate_val<'a>(&'a self, string: &str) -> Var<'a> {
        let ret = self.get(string);
        match ret {
            None => parse_to_yl_var(string),
            Some(var) => var.clone(),
        }
    }

    fn evaluate_list<'a>(&'a mut self, vec: &Vec<AstNode>, evaluate_function: bool) -> Var<'a> {
        if evaluate_function && vec.len() > 0 {
            let mut call_func = false;
            match vec[0] {
                AstNode::List(_) => {},
                AstNode::Val(ref fn_name) =>
                    match self.get(&fn_name) {
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

    fn evaluate_list_fallback<'a>(&'a mut self, vec: &Vec<AstNode>) -> Var<'a> {
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

    fn call<'a>(&'a mut self, vec: &Vec<AstNode>) -> Var<'a> {
        let context = CallContext::new(self, vec);
        println!("{:?}", context);
        Var::False
    }
}

fn parse_to_yl_var<'a>(string: &str) -> Var<'a> {
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
