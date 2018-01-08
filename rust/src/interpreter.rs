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

#[derive(Debug, Clone)]
pub enum FuncType {
    LetFn,
    UserDefined(UserDefinedFunc),
}

#[derive(Debug, Clone)]
pub struct Func {
    kind: FuncType,
    args: Vec<String>,
}

#[derive(Debug, Clone)]
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

impl Scope {
    pub fn global() -> ScopeContainer {
        let mut vars = HashMap::<String, Var>::new();
        vars.insert("let".to_string(), Var::Func(Func {
            kind: FuncType::LetFn,
            args: vec!["id".to_string(), "rhs".to_string()],
        }));
        ScopeContainer {
            scope: Rc::new(Scope {
                parent: None,
                vars: RefCell::new(vars),
            })
        }
    }

    fn extend(scope_ref: Rc<Scope>) -> Scope {
        Scope {
            parent: Some(scope_ref.clone()),
            vars: RefCell::new(HashMap::<String, Var>::new()),
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

    fn evaluate_list(&self, vec: &Vec<AstNode>, evaluate_function: bool) -> Var {
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

    fn evaluate_list_fallback(&self, vec: &Vec<AstNode>) -> Var {
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

    fn call(&self, vec: &Vec<AstNode>) -> Var {
        // let mut context = CallContext::new(self, vec);
        // context.call(vec)
        Var::False
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
