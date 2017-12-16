use std::collections::HashMap;
use std::collections::LinkedList;

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


struct YlFunc {
    argv: u32,
    arg_names: Vec<String>,
    scope: YlScope,
}

pub enum YlVar {
    False,
    Num(f64),
    Str(String),
    Func(YlFunc)
}

pub fn evaluate(ast: &LinkedList<AstNode>) -> YlVar {
    let scope = YlScope::new(None);
    evaluate_in_scope(ast, &scope)
}

pub fn evaluate_in_scope(ast: &LinkedList<AstNode>, scope: &YlScope) -> YlVar {
    // TODO
    YlVar::False
}
