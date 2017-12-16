use std::collections::HashMap;

use parser::AstNode;

pub struct YlScope {
    vars: HashMap<String, YlVar>,
    parent: Option<Box<YlScope>>,
}

impl YlScope {
    pub fn new(parent: Option<Box<YlScope>>) -> YlScope {
        YlScope {
            vars: HashMap::<String, YlVar>::new(),
            parent
        }
    }
}


pub struct YlFunc {
    args: Vec<String>,
    scope: YlScope,
}

pub enum YlVar {
    False,
    Num(f64),
    Str(String),
    Func(YlFunc)
}

pub fn evaluate(ast: &Vec<AstNode>) -> YlVar {
    let scope = YlScope::new(None);
    evaluate_in_scope(ast, &scope)
}

pub fn evaluate_in_scope(ast: &Vec<AstNode>, scope: &YlScope) -> YlVar {
    // TODO
    YlVar::False
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
