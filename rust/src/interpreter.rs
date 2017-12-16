use std::collections::HashMap;
use std::collections::LinkedList;

use parser::AstNode;


struct YlFunc {
    argv: u32,
    arg_names: Vec<String>,
    scope: HashMap<String, YlVar>,
}

pub enum YlVar {
    False,
    Num(f64),
    Str(String),
    Func(YlFunc)
}

pub fn evaluate(ast: &LinkedList<AstNode>) -> YlVar {
    // TODO
    YlVar::False
}
