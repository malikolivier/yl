use std::collections::HashMap;

use parser::AstNode;

pub struct YlScope<'a> {
    vars: HashMap<String, YlVar<'a>>,
    parent: Option<&'a YlScope<'a>>,
}

impl<'a> YlScope<'a> {
    pub fn new(parent: Option<&'a YlScope>) -> YlScope<'a> {
        YlScope {
            vars: HashMap::<String, YlVar>::new(),
            parent
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
                    break
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
        None
    }

    fn set(&'a mut self, name: &str, value: YlVar<'a>) {
        self.vars.insert(name.to_string(), value);
    }
}


pub struct YlFunc<'a> {
    args: Vec<String>,
    scope: &'a YlScope<'a>,
}

pub enum YlVar<'a> {
    False,
    Num(f64),
    Str(String),
    Func(YlFunc<'a>)
}


pub fn evaluate_in_scope<'a>(ast: &AstNode, scope: &'a YlScope,
                             evaluate_function: bool) -> &'a YlVar<'a> {
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


fn evaluate_val<'a>(string: &str, scope: &'a YlScope) -> &'a YlVar<'a> {
    match scope.get(string) {
        None => parse_to_yl_var(string),
        Some(var) => var,
    }
}

fn evaluate_list<'a>(vec: &Vec<AstNode>, scope: &YlScope,
                     evaluate_function: bool) -> &'a YlVar<'a> {
    &YlVar::False
}


fn parse_to_yl_var<'a>(string: &str) -> &'a YlVar<'a> {
    &YlVar::False
}
