use parser::AstNode;


pub enum Var {
    False,
    Num(f64),
}

pub struct Scope {
    children: Vec<Scope>,
}

impl Scope {
    pub fn new(parent: Option<Scope>) -> Scope {
        Scope {
            children: Vec::<Scope>::new()
        }
    }

    pub fn evaluate(&mut self, ast: &AstNode, evaluate_function: bool) -> Var {
        Var::False
    }
}


pub fn print(var: &Var) {

}
