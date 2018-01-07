use std::collections::HashMap;

use parser::AstNode;


pub struct UserDefinedFunc {
    scope: Box<Scope>,
    ast: Box<AstNode>,
}

pub enum FuncType {
    LetFn,
    UserDefined(UserDefinedFunc),
}

pub struct Func {
    kind: FuncType,
    args: Vec<String>,
}

pub enum Var {
    False,
    Num(f64),
    Str(String),
    Func(Func)
}

pub struct Scope {
    parent: Option<Box<Scope>>,
    vars: HashMap<String, Var>,
}

impl Scope {
    pub fn global() -> Scope {
        let mut vars = HashMap::<String, Var>::new();
        Scope {
            parent: None,
            vars,
        }
    }

    pub fn evaluate(&mut self, ast: &AstNode, evaluate_function: bool) -> Var {
        Var::False
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
