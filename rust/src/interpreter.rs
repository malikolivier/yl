use std::collections::HashMap;
use std::str::FromStr;

use parser::AstNode;

pub struct YlScope<'a> {
    vars: HashMap<String, YlVar<'a>>,
    parent: Option<&'a YlScope<'a>>,
}

impl<'a> YlScope<'a> {
    pub fn new(parent: Option<&'a YlScope>) -> YlScope<'a> {
        match parent {
            None => {
                let mut vars = HashMap::<String, YlVar>::new();
                vars.insert("let".to_string(), YlVar::Func(YlFunc {
                    args: vec!["id", "rhs"],
                    scope: None
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

    fn extend(&'a self) -> YlScope<'a> {
        Self::new(Some(self))
    }

    fn get(&'a self, name: &str) -> Option<&'a YlVar> {
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

    fn set(&'a mut self, name: &str, value: YlVar<'a>) {
        self.vars.insert(name.to_string(), value);
    }
}

#[derive(Clone)]
pub struct YlFunc<'a> {
    args: Vec<&'a str>,
    scope: Option<&'a YlScope<'a>>,
}

#[derive(Clone)]
pub enum YlVar<'a> {
    False,
    Num(f64),
    Str(String),
    Func(YlFunc<'a>)
}


pub fn evaluate_in_scope<'a>(ast: &AstNode, scope: &'a YlScope,
                             evaluate_function: bool) -> YlVar<'a> {
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
                print!("(def ");
                // TODO
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


fn evaluate_val<'a>(string: &str, scope: &'a YlScope) -> YlVar<'a> {
    match scope.get(string) {
        None => parse_to_yl_var(string),
        Some(var) => var.clone(),
    }
}

fn evaluate_list<'a>(vec: &Vec<AstNode>, scope: &'a YlScope,
                     evaluate_function: bool) -> YlVar<'a> {
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


fn parse_to_yl_var<'a>(string: &str) -> YlVar<'a> {
    match f64::from_str(string) {
        Err(_) => {
            let s = string.to_string();
            YlVar::Str(s)
        },
        Ok(n) => YlVar::Num(n),
    }
}

fn run_function<'a>(fn_name: &str, val: &YlVar, scope: &YlScope) -> YlVar<'a> {
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
