use std::collections::HashMap;

use parser::AstNode;


pub struct UserDefinedFunc<'s> {
    scope: &'s Scope<'s>,
    ast: Box<AstNode>,
}

pub enum FuncType<'s> {
    LetFn,
    UserDefined(UserDefinedFunc<'s>),
}

pub struct Func<'s> {
    kind: FuncType<'s>,
    args: Vec<String>,
}

pub enum Var<'s> {
    False,
    Num(f64),
    Str(String),
    Func(Func<'s>)
}

pub struct Scope<'s> {
    parent: Option<&'s Scope<'s>>,
    vars: HashMap<String, Var<'s>>,
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
