use std::collections::LinkedList;

pub enum AstNode {
    String,
    AstNode,
}


pub fn parse(code: &str) -> LinkedList<AstNode> {
    // TODO
    LinkedList::<AstNode>::new()
}
