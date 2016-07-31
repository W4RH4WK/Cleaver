use std::cell::RefCell;
use std::collections::LinkedList;
use std::collections::HashMap;
use std::rc::Rc;

use pest::prelude::*;

use ::front::Position;
use ::front::SymbolTable;
use ::front::ast;

impl_rdp! {
    grammar! {
        alpha = _{ ['A'..'Z'] | ['a'..'z'] }
        alpha_num  = _{ alpha | ['0'..'9'] }

// ------------------------------------------------------------ Base

        identifier = @{ alpha ~ (alpha_num | ["_"])* }

        bool_literal = { ["true"] | ["false"] }

        int_literal =  @{ ['0'..'9']+ }

        float_literal = @{ ['0'..'9']+ ~ ["."] ~ ['0'..'9']+ }

        string_literal = { ["\""] ~ (!["\""] ~ any)* ~ ["\""] }

        type_ = { ["void"] | ["bool"] | ["int"] | ["float"] | ["string"] }

        variable = { identifier }

        declaration = { type_ ~ variable }

        wrap_decl = { declaration }

        decl_list = _{ wrap_decl ~ ([","] ~ wrap_decl)* }

        literal = { bool_literal | float_literal | int_literal | string_literal }

// ------------------------------------------------------------ Operators

        rel_op = { ["=="] | ["<="] | ["<"] }

        add_op = { ["+"] | ["-"] }

        mul_op = { ["*"] | ["/"] }

        un_op = { ["-"] | ["!"] }

// ------------------------------------------------------------ Expressions

        expression = { simple_expression ~ (rel_op ~ simple_expression)? }

        simple_expression = { term ~ (add_op ~ simple_expression)? }

        term = { factor ~ (mul_op ~ term)? }

        factor = _{ un_op ~ expression
                  | literal_expr
                  | call_expr
                  | variable_expr
                  | parenth_expr
        }

        literal_expr = { literal }

        call_expr = { identifier ~ ["("] ~ expr_list? ~ [")"] }

        expr_list = _{ wrap_expr ~ ([","] ~ wrap_expr)* }

        wrap_expr = { expression }

        variable_expr = { variable }

        parenth_expr = { ["("] ~ expression ~ [")"] }

// ------------------------------------------------------------ Statements

        statement = _{ decl_stmt
                     | assignment
                     | if_stmt
                     | while_stmt
                     | return_stmt
                     | compound_stmt
                     | expr_stmt
        }

        expr_stmt = { expression ~ [";"] }

        decl_stmt = { declaration ~ [";"] }

        assignment = { variable ~ ["="] ~ expression ~ [";"] }

        if_stmt = { ["if"] ~ ["("] ~ expression ~ [")"] ~ statement ~ else_part? }

        else_part = { ["else"] ~ statement }

        while_stmt = { ["while"] ~ ["("] ~ expression ~ [")"] ~ statement }

        return_stmt = { ["return"] ~ wrap_expr? ~ [";"] }

        compound_stmt = { ["{"] ~ wrap_stmt* ~ ["}"] }

        wrap_stmt = { statement }

// ------------------------------------------------------------ Program

        program = { wrap_fun* }

        function = { type_ ~ identifier ~ ["("] ~ (decl_list)? ~ [")"] ~ statement }

        wrap_fun = { function }

// ------------------------------------------------------------ Comments / WS

        whitespace = _{ [" "] | ["\n" ] | ["\r"] | ["\t"] }
    }

    process! {
        parse_type(&self) -> ast::Type {
            (&t: type_) => match t {
                "void" => ast::Type::Void,
                "bool" => ast::Type::Bool,
                "int" => ast::Type::Int,
                "float" => ast::Type::Float,
                "string" => ast::Type::Str,
                _ => unreachable!(),
            }
        }

        parse_variable(&self) -> ast::Variable {
            (_: variable, &ident: identifier) => ast::Variable::new(ident.to_owned())
        }

        parse_literal(&self) -> ast::Literal {
            (_: literal, &v: bool_literal) => ast::Literal::Bool(match v {
                "true" => true,
                "false" => false,
                _ => unreachable!(),
            }),
            (_: literal, &v: int_literal) => ast::Literal::Int(v.parse::<i32>().unwrap()),
            (_: literal, &v: float_literal) => ast::Literal::Float(v.parse::<f32>().unwrap()),
            (_: literal, &v: string_literal) => ast::Literal::Str(v[1..v.len()-1].to_owned()),
       }

        parse_expression(&self) -> ast::Node<ast::Expression> {
            (pos: expression, lhs: parse_expression(), &op: rel_op, rhs: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Binary {
                        op: match op {
                            "==" => ast::BinaryOp::EQ,
                            "<=" => ast::BinaryOp::LE,
                            "<" => ast::BinaryOp::LT,
                            _ => unreachable!(),
                        },
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                },

            (_: expression, expr: parse_expression()) => expr,

            (pos: simple_expression, lhs: parse_expression(), &op: add_op, rhs: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Binary {
                        op: match op {
                            "+" => ast::BinaryOp::ADD,
                            "-" => ast::BinaryOp::SUB,
                            _ => unreachable!(),
                        },
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                },

            (_: simple_expression, expr: parse_expression()) => expr,

            (pos: term, lhs: parse_expression(), &op: mul_op, rhs: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Binary {
                        op: match op {
                            "*" => ast::BinaryOp::MUL,
                            "/" => ast::BinaryOp::DIV,
                            _ => unreachable!(),
                        },
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                },

            (_: term, expr: parse_expression()) => expr,

            (op: un_op, expr: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(op.start)),
                    node: ast::Expression::Unary {
                        op: match self.input().slice(op.start, op.end) {
                            "-" => ast::UnaryOp::MINUS,
                            "!" => ast::UnaryOp::NOT,
                            _ => unreachable!(),
                        },
                        expr: Box::new(expr),
                    },
                },

            (pos: parenth_expr, expr: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Parenthesis {
                        expr: Box::new(expr),
                    },
                },

            (pos: literal_expr, lit: parse_literal()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Literal {
                        lit: lit,
                    },
                },

            (pos: variable_expr, var: parse_variable()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Variable {
                        var: Rc::new(var),
                    },
                },

            (pos: call_expr, &fun: identifier, args: parse_expressions()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Call {
                        function: fun.to_owned(),
                        args: args.into_iter().collect(),
                    },
                },
        }

        parse_expressions(&self) -> LinkedList<ast::Node<ast::Expression>> {
            (_: wrap_expr, expr: parse_expression(), mut tail: parse_expressions()) => {
                tail.push_front(expr);
                tail
            },
            () => LinkedList::new(),
        }

        parse_statement(&self) -> ast::Node<ast::Statement> {
            (pos: expr_stmt, expr: parse_expression()) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::Expression {
                        expr: Box::new(expr),
                    }
                }
            },

            (pos: decl_stmt, var: parse_declaration()) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::Declaration {
                        var: var,
                    }
                }
            },

            (pos: assignment, var: parse_variable(), expr: parse_expression()) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::Assignment {
                        var: Rc::new(var),
                        expr: Box::new(expr),
                    }
                }
            },

            (pos: if_stmt, cond: parse_expression(), on_true: parse_statement(), on_false: parse_else_part()) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::If {
                        cond: Box::new(cond),
                        on_true: Box::new(on_true),
                        on_false: on_false,
                    }
                }
            },

            (pos: while_stmt, cond: parse_expression(), body: parse_statement()) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::While {
                        cond: Box::new(cond),
                        body: Box::new(body),
                    }
                }
            },

            (pos: return_stmt, _: wrap_expr, expr: parse_expression()) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::Return {
                        expr: Some(Box::new(expr)),
                    }
                }
            },

            (pos: return_stmt) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::Return {
                        expr: None,
                    }
                }
            },

            (pos: compound_stmt, stmts: parse_statements()) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::Compound {
                        stmts: stmts.into_iter().collect(),
                        symbols: Rc::new(RefCell::new(SymbolTable::new(None))),
                    }
                }
            },
        }

        parse_else_part(&self) -> Option<Box<ast::Node<ast::Statement>>> {
            (_: else_part, stmt: parse_statement()) => Some(Box::new(stmt)),
            () => None,
        }

        parse_statements(&self) -> LinkedList<ast::Node<ast::Statement>> {
            (_: wrap_stmt, stmt: parse_statement(), mut tail: parse_statements()) => {
                tail.push_front(stmt);
                tail
            },
            () => LinkedList::new(),
        }

        parse_function(&self) -> ast::Node<ast::Function> {
            (pos: function, ret_type: parse_type(), &name: identifier, args: parse_declarations(), body: parse_statement()) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Function {
                        name: name.to_owned(),
                        filename: "".to_owned(),
                        body: body,
                        args: args.into_iter().collect(),
                        ret_type: ret_type,
                        symbols: Rc::new(RefCell::new(SymbolTable::new(None))),
                    }
                }
            }
        }

        parse_functions(&self) -> HashMap<String, ast::Node<ast::Function>> {
            (_: wrap_fun, fun: parse_function(), mut tail: parse_functions()) => {
                tail.insert(fun.node.name.clone(), fun);
                tail
            },
            () => HashMap::new(),
        }

        parse_declaration(&self) -> Rc<ast::Variable> {
            (_: declaration, type_: parse_type(), mut var: parse_variable()) => {
                var.type_ = type_;
                Rc::new(var)
            },
        }

        parse_declarations(&self) -> LinkedList<Rc<ast::Variable>> {
            (_: wrap_decl, var: parse_declaration(), mut tail: parse_declarations()) => {
                tail.push_front(var);
                tail
            },
            () => LinkedList::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pest::prelude::*;
    use ::front::ast;

    #[test]
    fn variables() {
        let mut parser = Rdp::new(StringInput::new("foo b4r ba_"));
        assert!(parser.variable());
        assert!(ast::Variable::new("foo".to_owned()).eq_name_type(&parser.parse_variable()));
        assert!(parser.variable());
        assert!(ast::Variable::new("b4r".to_owned()).eq_name_type(&parser.parse_variable()));
        assert!(parser.variable());
        assert!(ast::Variable::new("ba_".to_owned()).eq_name_type(&parser.parse_variable()));
        assert!(parser.end());
    }

    #[test]
    fn literals() {
        // bool
        let mut parser = Rdp::new(StringInput::new("true false"));
        assert!(parser.literal());
        assert_eq!(ast::Literal::Bool(true), parser.parse_literal());
        assert!(parser.literal());
        assert_eq!(ast::Literal::Bool(false), parser.parse_literal());
        assert!(parser.end());

        // int
        let mut parser = Rdp::new(StringInput::new("0 10 012"));
        assert!(parser.literal());
        assert_eq!(ast::Literal::Int(0), parser.parse_literal());
        assert!(parser.literal());
        assert_eq!(ast::Literal::Int(10), parser.parse_literal());
        assert!(parser.literal());
        assert_eq!(ast::Literal::Int(12), parser.parse_literal());
        assert!(parser.end());

        // float
        let mut parser = Rdp::new(StringInput::new("0.0 0.10 10.0 12.12"));
        assert!(parser.literal());
        assert_eq!(ast::Literal::Float(0.0), parser.parse_literal());
        assert!(parser.literal());
        assert_eq!(ast::Literal::Float(0.10), parser.parse_literal());
        assert!(parser.literal());
        assert_eq!(ast::Literal::Float(10.0), parser.parse_literal());
        assert!(parser.literal());
        assert_eq!(ast::Literal::Float(12.12), parser.parse_literal());
        assert!(parser.end());

        // string
        let mut parser = Rdp::new(StringInput::new("\"foo\" \"b4r\" \"\""));
        assert!(parser.literal());
        assert_eq!(ast::Literal::Str("foo".to_owned()), parser.parse_literal());
        assert!(parser.literal());
        assert_eq!(ast::Literal::Str("b4r".to_owned()), parser.parse_literal());
        assert!(parser.literal());
        assert_eq!(ast::Literal::Str("".to_owned()), parser.parse_literal());
        assert!(parser.end());
    }

    #[test]
    fn types() {
        let mut parser = Rdp::new(StringInput::new("void bool int float string"));
        assert!(parser.type_());
        assert_eq!(ast::Type::Void, parser.parse_type());
        assert!(parser.type_());
        assert_eq!(ast::Type::Bool, parser.parse_type());
        assert!(parser.type_());
        assert_eq!(ast::Type::Int, parser.parse_type());
        assert!(parser.type_());
        assert_eq!(ast::Type::Float, parser.parse_type());
        assert!(parser.type_());
        assert_eq!(ast::Type::Str, parser.parse_type());
        assert!(parser.end());
    }

    #[test]
    fn expressions() {
        // TODO
    }
}
