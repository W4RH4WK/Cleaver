use std::rc::Rc;

#[derive(PartialEq, Debug)]
pub struct Position {
    line: i32,
    col: i32,
}

#[derive(PartialEq, Debug)]
pub struct Node<I> {
    pos: Position,
    node: I
}

#[derive(PartialEq, Debug)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    Float(f32),
}

impl Literal {
    fn get_type(&self) -> Type {
        match *self {
            Literal::Bool(_) => Type::Bool,
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Variable {
    type_: Type,
    name: String,
}

#[derive(PartialEq, Debug)]
pub enum UnaryOp {
    NOT,
    MINUS,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOp {
    ADD,
    SUB,
    MUL,
    DIV,

    EQ,
    LT,
    LE
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Declaration {
        var: Rc<Node<Variable>>,
        init: Box<Node<Expression>>,
    },
    If {
        cond: Box<Node<Expression>>,
        on_true: Box<Node<Statement>>,
        on_false: Option<Box<Node<Statement>>>,
    },
    While {
        cond: Box<Node<Expression>>,
        body: Box<Node<Statement>>,
    },
    Return {
        expr: Box<Node<Expression>>,
    },
    Compound {
        stmts: Box<Vec<Node<Statement>>>,
    }
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Parenthesis {
        expr: Box<Node<Expression>>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Node<Expression>>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Node<Expression>>,
        right: Box<Node<Expression>>,
    },
    Literal {
        lit: Literal,
    },
    Variable {
        var: Rc<Node<Variable>>,
    },
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn literals() {
        assert_eq!(Type::Bool, Literal::Bool(false).get_type());
        assert_eq!(Type::Int, Literal::Int(0).get_type());
        assert_eq!(Type::Float, Literal::Float(0.0).get_type());
    }
}
