use std::collections::HashMap;
use std::str::FromStr;

use parser::AstNode;

pub struct YlScope<'s, 'p: 's> {
    vars: HashMap<String, YlVar<'s, 's, 's>>,
    parent: Option<&'s YlScope<'p, 'p>>,
}

impl<'s, 'p> YlScope<'s, 'p> {
    pub fn new(parent: Option<&'p YlScope>) -> YlScope<'s, 'p> {
        match parent {
            None => {
                let mut vars = HashMap::<String, YlVar>::new();
                vars.insert("let".to_string(), YlVar::Func(YlFunc {
                    args: vec!["id", "rhs"],
                    kind: FuncType::LetFn,
                }));
                YlScope { vars, parent }
            },
            Some(scope) => {
                YlScope {
                    vars: HashMap::<String, YlVar>::new(),
                    parent
                }
            },
        }
    }

    fn extend(&'p self) -> YlScope<'s, 'p> {
        Self::new(Some(self))
    }

    fn get(&'s self, name: &str) -> Option<&'s YlVar> {
        let mut parent = Some(self);
        loop {
            match parent {
                None => {
                    return None;
                },
                Some(scope) =>
                    match scope.vars.get(name) {
                        None => {
                            parent = parent.unwrap().parent;
                        },
                        Some(var) => {
                            return Some(var);
                        },
                    }
            }
        }
    }

    fn set(&'s mut self, name: &str, value: YlVar<'s, 's, 's>) {
        self.vars.insert(name.to_string(), value);
    }
}


#[derive(Clone)]
pub struct UserDefinedFunc<'s, 'p: 's> {
    scope: Option<&'s YlScope<'s, 'p>>,
}

#[derive(Clone)]
pub enum FuncType<'s, 'p: 's> {
    LetFn,
    UserDefined(UserDefinedFunc<'s, 'p>),
}

#[derive(Clone)]
pub struct YlFunc<'a, 's: 'a, 'p: 's> {
    kind: FuncType<'s, 'p>,
    args: Vec<&'a str>,
}

#[derive(Clone)]
pub enum YlVar<'a, 's: 'a, 'p: 's> {
    False,
    Num(f64),
    Str(String),
    Func(YlFunc<'a, 's, 'p>)
}


pub fn evaluate_in_scope<'s>(ast: &AstNode, scope: &'s YlScope,
                             evaluate_function: bool) -> YlVar<'s, 's, 's> {
    match ast {
        &AstNode::Val(ref string) => evaluate_val(string, scope),
        &AstNode::List(ref vec) => evaluate_list(&vec, scope, evaluate_function),
    }
}

fn print_fn(argv: Vec<&YlVar>) {
    for var in argv.iter() {
        match *var {
            &YlVar::False => println!("()"),
            &YlVar::Num(n) => println!("{}", n),
            &YlVar::Str(ref s) => println!("{}", s),
            &YlVar::Func(ref f) => {
                print!("(def function (");
                for arg in f.args.clone() {
                    print!("{} ", arg);
                }
                print!(")");
                match f.kind.clone() {
                    FuncType::UserDefined(f) => {
                        print!(" ... "); // TODO
                    },
                    _ => print!(" [native code] "),
                }
                println!(")");
            },
        }
    }
}

pub fn print(var: &YlVar) {
    let mut vec = Vec::<&YlVar>::new();
    vec.push(var);
    print_fn(vec)
}


fn evaluate_val<'s>(string: &str, scope: &'s YlScope) -> YlVar<'s, 's, 's> {
    match scope.get(string) {
        None => parse_to_yl_var(string),
        Some(var) => var.clone(),
    }
}

fn evaluate_list<'s>(vec: &Vec<AstNode>, scope: &'s YlScope,
                     evaluate_function: bool) -> YlVar<'s, 's, 's> {
    if evaluate_function && vec.len() > 0 {
        match vec[0] {
            AstNode::List(_) => {},
            AstNode::Val(ref fn_name) =>
                match scope.get(&fn_name) {
                    None => {},
                    Some(func) => {
                        return run_function(&fn_name, func, scope);
                    },
                },
        }
    }
    let mut ret = YlVar::False;
    for node in vec {
        ret = evaluate_in_scope(node, scope, true);
    }
    ret
}


fn parse_to_yl_var<'a>(string: &str) -> YlVar<'a, 'a, 'a> {
    match f64::from_str(string) {
        Err(_) => {
            let s = string.to_string();
            YlVar::Str(s)
        },
        Ok(n) => YlVar::Num(n),
    }
}

fn run_function<'s>(fn_name: &str, val: &YlVar, scope: &'s YlScope) -> YlVar<'s, 's, 's> {
    match fn_name {
        "let" => {
            // TODO
            eprintln!("'let' not implemented");
            YlVar::False
        },
        string => {
            eprintln!("Normal function not implemented");
            YlVar::False
        },
    }
}
