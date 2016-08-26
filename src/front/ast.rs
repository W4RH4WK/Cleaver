use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::Position;
use super::symbols::SymbolTable;

pub type Functions = HashMap<String, Node<Function>>;

#[derive(PartialEq, Debug)]
pub struct Node<I> {
    pub pos: Position,
    pub node: I,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
    Str,
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    Float(f32),
    Str(String),
}

impl Literal {
    pub fn get_type(&self) -> Type {
        match *self {
            Literal::Bool(_) => Type::Bool,
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Str(_) => Type::Str,
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    pub id: u32,
    pub type_: Type,
    pub name: String,
}

impl Variable {
    pub fn stub(name: String) -> Variable {
        Variable {
            id: 0,
            type_: Type::Void,
            name: name,
        }
    }

    pub fn eq_name_type(&self, other: &Variable) -> bool {
        self.type_ == other.type_ && self.name == other.name
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Variable) -> bool {
        if self.id == 0 || other.id == 0 {
            false
        } else {
            self.id == other.id
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum UnaryOp {
    NOT,
    MINUS,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum BinaryOp {
    ADD,
    SUB,
    MUL,
    DIV,

    EQ,
    LT,
    LE,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
}

#[derive(PartialEq, Debug)]
pub struct Program {
    pub functions: Functions,
}

#[derive(PartialEq, Debug)]
pub struct Function {
    pub name: String,
    pub filename: String,
    pub body: Node<Statement>,
    pub args: Vec<Rc<Variable>>,
    pub ret_type: Type,
    pub symbols: Rc<RefCell<SymbolTable>>,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Expression {
        expr: Box<Node<Expression>>,
    },
    Declaration {
        var: Rc<Variable>,
    },
    Assignment {
        var: Rc<Variable>,
        expr: Box<Node<Expression>>,
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
        expr: Option<Box<Node<Expression>>>,
    },
    Compound {
        stmts: Vec<Node<Statement>>,
        symbols: Rc<RefCell<SymbolTable>>,
    },
}

impl Node<Statement> {
    /// Visit all statements with a given closure.
    ///
    /// The given closure is called for each statement with a reference to the
    /// given node. This reference inherits the lifetime of `self`. The return
    /// value indicates whether further statements should be visited or not.
    ///
    /// Use this method as alternative to the [Visitor].
    ///
    /// [Visitor]: <trait.Visitor.html>
    pub fn visit_stmt<'a>(&'a self, do_stmt: &mut FnMut(&'a Node<Statement>) -> bool) -> bool {
        do_stmt(self) || return false;
        match self.node {
            Statement::If { ref on_true, ref on_false, .. } => {
                on_true.visit_stmt(do_stmt) ||
                if let Some(ref stmt) = *on_false {
                    stmt.visit_stmt(do_stmt)
                } else {
                    true
                }
            }
            Statement::While { ref body, .. } => body.visit_stmt(do_stmt),
            Statement::Compound { ref stmts, .. } => {
                for stmt in stmts.iter() {
                    stmt.visit_stmt(do_stmt) || return false;
                }
                true
            }
            _ => true,
        }
    }

    /// Visit all expressions with a given closure.
    ///
    /// Same as [visit_stmt], but for expressions.
    ///
    /// [visit_stmt]: <struct.Node.html#method.visit_expr>
    pub fn visit_expr<'a>(&'a self, do_expr: &mut FnMut(&'a Node<Expression>) -> bool) -> bool {
        self.visit_stmt(&mut |stmt| {
            match stmt.node {
                Statement::Expression { ref expr } |
                Statement::Assignment { ref expr, .. } |
                Statement::Return { expr: Some(ref expr) } => do_expr(expr),
                Statement::If { ref cond, .. } |
                Statement::While { ref cond, .. } => do_expr(cond),
                _ => true,
            }
        })
    }
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Literal {
        lit: Literal,
    },
    Variable {
        var: Rc<Variable>,
    },
    Call {
        function: String,
        args: Vec<Node<Expression>>,
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
    Parenthesis {
        expr: Box<Node<Expression>>,
    },
}

impl Node<Expression> {
    /// Visit all expressions with a given closure.
    ///
    /// Same as [visit_stmt], but for expressions.
    ///
    /// [visit_stmt]: <struct.Node.html#method.visit_expr>
    pub fn visit_expr<'a>(&'a self, do_expr: &mut FnMut(&'a Node<Expression>) -> bool) -> bool {
        do_expr(self) || return false;
        match self.node {
            Expression::Call { ref args, .. } => {
                for arg in args.iter() {
                    arg.visit_expr(do_expr) || return false;
                }
                true
            }
            Expression::Unary { ref expr, .. } |
            Expression::Parenthesis { ref expr } => expr.visit_expr(do_expr),
            Expression::Binary { ref left, ref right, .. } => {
                left.visit_expr(do_expr) || return false;
                right.visit_expr(do_expr)
            }
            _ => true,
        }
    }
}

/// Enables visiting specific nodes in the AST.
///
/// Source: <https://github.com/darwins-challenge/moonlander-ast-rust>
pub trait Visitor<'a> {
    /// Continue visiting nodes?
    fn cont(&self) -> bool {
        true
    }

    /// Handler for Statements
    fn visit_stmt(&mut self, &'a Node<Statement>) {}

    /// Handler for Expressions
    fn visit_expr(&mut self, &'a Node<Expression>) {}
}

/// Enables something to be visited by a [Visitor], intended for AST nodes.
///
/// [Visitor]: <trait.Visitor.html>
pub trait Visitable<'a> {
    fn visit(&'a self, visitor: &mut Visitor<'a>);
}

impl<'a> Visitable<'a> for Node<Function> {
    fn visit(&'a self, visitor: &mut Visitor<'a>) {
        self.node.body.visit(visitor);
    }
}

impl<'a> Visitable<'a> for Node<Statement> {
    fn visit(&'a self, visitor: &mut Visitor<'a>) {
        if !visitor.cont() {
            return;
        }

        visitor.visit_stmt(self);

        match self.node {
            Statement::Expression { ref expr } |
            Statement::Assignment { ref expr, .. } |
            Statement::Return { expr: Some(ref expr) } => expr.visit(visitor),
            Statement::If { ref cond, ref on_true, ref on_false } => {
                cond.visit(visitor);
                on_true.visit(visitor);
                if let Some(ref stmt) = *on_false {
                    stmt.visit(visitor);
                }
            }
            Statement::While { ref cond, ref body } => {
                cond.visit(visitor);
                body.visit(visitor);
            }
            Statement::Compound { ref stmts, .. } => {
                for stmt in stmts {
                    stmt.visit(visitor);
                }
            }
            _ => (),
        }
    }
}

impl<'a> Visitable<'a> for Node<Expression> {
    fn visit(&'a self, visitor: &mut Visitor<'a>) {
        if !visitor.cont() {
            return;
        }

        visitor.visit_expr(self);

        match self.node {
            Expression::Call { ref args, .. } => {
                for arg in args {
                    arg.visit(visitor);
                }
            }
            Expression::Binary { ref left, ref right, .. } => {
                left.visit(visitor);
                right.visit(visitor);
            }
            Expression::Unary { ref expr, .. } |
            Expression::Parenthesis { ref expr } => expr.visit(visitor),
            _ => (),
        }
    }
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
