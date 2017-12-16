use std::collections::LinkedList;

use parser::AstNode;


pub enum YlType {
    False,
    Number,
    String,
    Function,
}

pub struct YlVar {
    pub kind: YlType,
}

pub fn evaluate(ast: &LinkedList<AstNode>) -> YlVar {
    YlVar {
        kind: YlType::False
    }
}
