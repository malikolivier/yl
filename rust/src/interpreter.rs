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

    fn get(&'a self, name: &str) -> &'a YlVar {
        let mut parent = Some(self);
        let mut ret = &YlVar::False;
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
                            ret = var;
                            break
                        },
                    }
            }
        }
        ret
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

pub fn evaluate(ast: &AstNode) -> YlVar {
    let scope = YlScope::new(None);
    evaluate_in_scope(ast, &scope, true)
}

pub fn evaluate_in_scope<'a>(ast: &AstNode, scope: &YlScope,
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


fn evaluate_val<'a, 'b>(string: &str, scope: &'b YlScope) -> YlVar<'a> {
    YlVar::False
}

fn evaluate_list<'a, 'b>(vec: &Vec<AstNode>, scope: &'b YlScope,
                         evaluate_function: bool) -> YlVar<'a> {
    YlVar::False
}
