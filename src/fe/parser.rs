#![cfg_attr(feature="clippy", allow(block_in_if_condition_stmt))]

use std::collections::LinkedList;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::rc::Rc;

use pest::prelude::*;

use ::diag;
use super::ast;
use super::Position;

pub fn parse(input: &str) -> ast::Functions {
    let mut parser = Rdp::new(StringInput::new(input));
    assert!(parser.program());
    parser.parse_program()
}

pub fn parse_file(filepath: &Path, config: &Option<diag::Config>) -> ast::Functions {
    // get file content
    let mut content = String::new();
    {
        let mut file = File::open(filepath).unwrap();
        file.read_to_string(&mut content).unwrap();
    }

    // run parser
    let mut functions;
    {
        let mut parser = Rdp::new(StringInput::new(&content));
        assert!(parser.program());

        // output tokens
        if config.as_ref().map_or(false, |c| c.dump_tokens) {
            // filepath
            let token_filepath = config.as_ref()
                .unwrap()
                .output_dir()
                .join(format!("tokens_{}.txt",
                              filepath.file_name().unwrap().to_str().unwrap()));

            // dump tokens
            Write::write_all(&mut File::create(token_filepath.as_path()).unwrap(),
                             format!("{:?}", parser.queue()).as_bytes());
        }

        functions = parser.parse_program()
    }

    // set filename
    for (_, f) in &mut functions {
        f.node.filename = filepath.file_name().unwrap().to_str().unwrap().to_owned();
    }

    functions
}

pub fn parse_files(filepaths: &[&Path], config: &Option<diag::Config>) -> ast::Functions {
    let mut functions = ast::Functions::new();

    // parse all input files
    for filepath in filepaths {
        for (name, function) in parse_file(filepath, config) {
            functions.insert(name, function);
        }
    }

    functions
}

impl_rdp! {
    grammar! {
        alpha = _{ ['A'..'Z'] | ['a'..'z'] }
        alpha_num  = _{ alpha | ['0'..'9'] }

// ------------------------------------------------------------ Base

        identifier = @{ alpha ~ (alpha_num | ["_"])* }

        bool_literal = _{ bool_literal_true | bool_literal_false }
        bool_literal_true = { ["true"] }
        bool_literal_false = { ["false"] }

        int_literal =  @{ ['0'..'9']+ }

        float_literal = @{ ['0'..'9']+ ~ ["."] ~ ['0'..'9']+ }

        string_literal = { ["\""] ~ (!["\""] ~ any)* ~ ["\""] }

        type_ = { type_void | type_bool | type_int | type_float | type_string }
        type_void = { ["void"] }
        type_bool = { ["bool"] }
        type_int = { ["int"] }
        type_float = { ["float"] }
        type_string = { ["string"] }

        variable = { identifier }

        declaration = { type_ ~ variable }

        wrap_decl = { declaration }

        decl_list = _{ wrap_decl ~ ([","] ~ wrap_decl)* }

        literal = { bool_literal | float_literal | int_literal | string_literal }

// ------------------------------------------------------------ Operators

        rel_op = { rel_op_eq | rel_op_le | rel_op_lt }
        rel_op_eq = { ["=="] }
        rel_op_le = { ["<="] }
        rel_op_lt = { ["<"] }

        add_op = { add_op_add | add_op_sub }
        add_op_add = { ["+"] }
        add_op_sub = { ["-"] }

        mul_op = { mul_op_mul | mul_op_div }
        mul_op_mul = { ["*"] }
        mul_op_div = { ["/"] }

        un_op = { un_op_minus | un_op_not }
        un_op_minus = { ["-"] }
        un_op_not = { ["!"] }

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

        call_expr = { identifier ~ lparenth ~ expr_list? ~ rparenth }

        expr_list = _{ wrap_expr ~ ([","] ~ wrap_expr)* }

        wrap_expr = { expression }

        variable_expr = { variable }

        parenth_expr = { lparenth ~ expression ~ rparenth }

        lparenth = { ["("] }

        rparenth = { [")"] }

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

        compound_stmt = { lbrace ~ wrap_stmt* ~ rbrace }

        lbrace = { ["{"] }

        rbrace = { ["}"] }

        wrap_stmt = { statement }

// ------------------------------------------------------------ Program

        program = _{ wrap_fun+ }

        function = { type_ ~ identifier ~ ["("] ~ (decl_list)? ~ [")"] ~ statement }

        wrap_fun = { function }

// ------------------------------------------------------------ Comments / WS

        whitespace = _{ [" "] | ["\n" ] | ["\r"] | ["\t"] }
    }

    process! {
        parse_variable(&self) -> ast::Variable {
            (_: variable, &ident: identifier) => ast::Variable::stub(ident.to_owned())
        }

        parse_type(&self) -> ast::Type {
            (_: type_, _: type_void) => ast::Type::Void,
            (_: type_, _: type_bool) => ast::Type::Bool,
            (_: type_, _: type_int) => ast::Type::Int,
            (_: type_, _: type_float) => ast::Type::Float,
            (_: type_, _: type_string) => ast::Type::Str,
        }

        parse_binary_op(&self) -> ast::BinaryOp {
            (_: rel_op_eq) => ast::BinaryOp::EQ,
            (_: rel_op_le) => ast::BinaryOp::LE,
            (_: rel_op_lt) => ast::BinaryOp::LT,
            (_: add_op_add) => ast::BinaryOp::ADD,
            (_: add_op_sub) => ast::BinaryOp::SUB,
            (_: mul_op_mul) => ast::BinaryOp::MUL,
            (_: mul_op_div) => ast::BinaryOp::DIV,
        }

        parse_unary_op(&self) -> ast::UnaryOp {
            (_: un_op_minus) => ast::UnaryOp::MINUS,
            (_: un_op_not) => ast::UnaryOp::NOT,
        }

        parse_literal(&self) -> ast::Literal {
            (_: literal, _: bool_literal_true) => ast::Literal::Bool(true),
            (_: literal, _: bool_literal_false) => ast::Literal::Bool(false),
            (_: literal, &v: int_literal) => ast::Literal::Int(v.parse::<i32>().unwrap()),
            (_: literal, &v: float_literal) => ast::Literal::Float(v.parse::<f32>().unwrap()),
            (_: literal, &v: string_literal) => ast::Literal::Str(v[1..v.len()-1].to_owned()),
       }

        parse_expression(&self) -> ast::Node<ast::Expression> {
            (pos: expression, lhs: parse_expression(), _: rel_op, op: parse_binary_op(), rhs: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Binary {
                        op: op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                },

            (_: expression, expr: parse_expression()) => expr,

            (pos: simple_expression, lhs: parse_expression(), _: add_op, op: parse_binary_op(), rhs: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Binary {
                        op: op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                },

            (_: simple_expression, expr: parse_expression()) => expr,

            (pos: term, lhs: parse_expression(), _: mul_op, op: parse_binary_op(), rhs: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Binary {
                        op: op ,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                },

            (_: term, expr: parse_expression()) => expr,

            (pos: un_op, op: parse_unary_op(), expr: parse_expression()) =>
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Expression::Unary {
                        op: op,
                        expr: Box::new(expr),
                    },
                },

            (pos: parenth_expr, _: lparenth, expr: parse_expression(), _: rparenth) =>
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

            (pos: call_expr, &fun: identifier, _: lparenth, args: parse_expressions(), _:rparenth) =>
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

            (pos: compound_stmt, _: lbrace, stmts: parse_statements(), _: rbrace) => {
                ast::Node {
                    pos: Position::from(self.input().line_col(pos.start)),
                    node: ast::Statement::Compound {
                        stmts: stmts.into_iter().collect(),
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
                        ret_type: ret_type
                    }
                }
            }
        }

        parse_program(&self) -> ast::Functions {
            (_: wrap_fun, fun: parse_function(), mut tail: parse_program()) => {
                tail.insert(fun.node.name.clone(), fun);
                tail
            },
            () => ast::Functions::new(),
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
    pub use super::*;
    pub use pest::prelude::*;
    pub use ::frontend::ast;

    #[test]
    fn variables() {
        let mut parser = Rdp::new(StringInput::new("foo b4r ba_"));
        assert!(parser.variable());
        assert!(ast::Variable::stub("foo".to_owned()).eq_name_type(&parser.parse_variable()));
        assert!(parser.variable());
        assert!(ast::Variable::stub("b4r".to_owned()).eq_name_type(&parser.parse_variable()));
        assert!(parser.variable());
        assert!(ast::Variable::stub("ba_".to_owned()).eq_name_type(&parser.parse_variable()));
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
    fn function() {
        let mut parser = Rdp::new(StringInput::new("int foo(int bar, float baz) return bar;"));
        parser.function();

        let ast::Function { name, body, args, ret_type, .. } = parser.parse_function().node;

        assert_eq!("foo", name);
        assert_eq!(ast::Type::Int, ret_type);

        // check arguments
        assert_eq!(2, args.len());
        let arg0 = ast::Variable {
            id: 0,
            name: "bar".to_owned(),
            type_: ast::Type::Int,
        };
        assert!(arg0.eq_name_type(&args[0]));
        let arg1 = ast::Variable {
            id: 0,
            name: "baz".to_owned(),
            type_: ast::Type::Float,
        };
        assert!(arg1.eq_name_type(&args[1]));

        // check body
        match body.node {
            ast::Statement::Return { ref expr } => {
                match expr.as_ref().unwrap().node {
                    ast::Expression::Variable { ref var } => assert_eq!("bar", var.name),
                    _ => assert!(false),
                }
            }
            _ => assert!(false),
        };

        assert!(parser.end());
    }

    #[test]
    fn program() {
        let fs = &vec![
            "int foo(int bar, float baz) { return bar; }",
            "bool boo(string taf, int taz) { 2 + 3; return taz; }",
        ]
            .join("");

        let mut parser = Rdp::new(StringInput::new(fs));
        parser.program();
        assert_eq!(2, parser.parse_program().len());
        assert!(parser.end());
    }

    mod literal {
        use super::*;

        #[test]
        fn bool() {
            // bool
            let mut parser = Rdp::new(StringInput::new("true false"));
            assert!(parser.literal());
            assert_eq!(ast::Literal::Bool(true), parser.parse_literal());
            assert!(parser.literal());
            assert_eq!(ast::Literal::Bool(false), parser.parse_literal());
            assert!(parser.end());
        }

        #[test]
        fn int() {
            // int
            let mut parser = Rdp::new(StringInput::new("0 10 012"));
            assert!(parser.literal());
            assert_eq!(ast::Literal::Int(0), parser.parse_literal());
            assert!(parser.literal());
            assert_eq!(ast::Literal::Int(10), parser.parse_literal());
            assert!(parser.literal());
            assert_eq!(ast::Literal::Int(12), parser.parse_literal());
            assert!(parser.end());
        }

        #[test]
        fn float() {
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
        }

        #[test]
        fn string() {
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
    }

    mod expression {
        use super::*;

        #[test]
        fn literal() {
            let mut parser = Rdp::new(StringInput::new("2"));
            assert!(parser.expression());
            assert_eq!(ast::Expression::Literal { lit: ast::Literal::Int(2) },
                       parser.parse_expression().node);
            assert!(parser.end());
        }

        #[test]
        fn variable() {
            let mut parser = Rdp::new(StringInput::new("foo"));
            assert!(parser.expression());
            match parser.parse_expression().node {
                ast::Expression::Variable { var } => assert_eq!("foo", var.name),
                _ => assert!(false),
            }
            assert!(parser.end());
        }

        #[test]
        fn call() {
            let mut parser = Rdp::new(StringInput::new("foo(2)"));
            assert!(parser.expression());
            match parser.parse_expression().node {
                ast::Expression::Call { function, args } => {
                    assert_eq!("foo", function);
                    assert_eq!(1, args.len());
                    assert_eq!(ast::Expression::Literal { lit: ast::Literal::Int(2) },
                               args[0].node)
                }
                _ => assert!(false),
            }
            assert!(parser.end());
        }

        #[test]
        fn unary() {
            let mut parser = Rdp::new(StringInput::new("(-2)"));
            assert!(parser.expression());
            match parser.parse_expression().node {
                ast::Expression::Parenthesis { expr } => {
                    match expr.node {
                        ast::Expression::Unary { ref op, ref expr } => {
                            assert_eq!(&ast::UnaryOp::MINUS, op);
                            match expr.node {
                                ast::Expression::Literal { ref lit } => {
                                    assert_eq!(&ast::Literal::Int(2), lit)
                                }
                                _ => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
            assert!(parser.end());
        }

        #[test]
        fn binary() {
            let mut parser = Rdp::new(StringInput::new("(2 + 3)"));
            assert!(parser.expression());
            match parser.parse_expression().node {
                ast::Expression::Parenthesis { expr } => {
                    match expr.node {
                        ast::Expression::Binary { ref op, ref left, ref right } => {
                            assert_eq!(&ast::BinaryOp::ADD, op);
                            match left.node {
                                ast::Expression::Literal { ref lit } => {
                                    assert_eq!(&ast::Literal::Int(2), lit)
                                }
                                _ => assert!(false),
                            };
                            match right.node {
                                ast::Expression::Literal { ref lit } => {
                                    assert_eq!(&ast::Literal::Int(3), lit)
                                }
                                _ => assert!(false),
                            };
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
            assert!(parser.end());
        }
    }

    mod statement {
        use super::*;

        static L: ast::Literal = ast::Literal::Int(2);

        fn v() -> ast::Variable {
            ast::Variable {
                id: 0,
                type_: ast::Type::Int,
                name: "foo".to_owned(),
            }
        }

        #[test]
        fn expression() {
            let mut parser = Rdp::new(StringInput::new("2;"));
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::Expression { ref expr } => {
                    match expr.node {
                        ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
            assert!(parser.end());
        }

        #[test]
        fn declaration() {
            let mut parser = Rdp::new(StringInput::new("int foo;"));
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::Declaration { ref var } => assert!(v().eq_name_type(var)),
                _ => assert!(false),
            }
            assert!(parser.end());
        }

        #[test]
        fn assignment() {
            let mut parser = Rdp::new(StringInput::new("foo = 2;"));
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::Assignment { ref var, ref expr } => {
                    assert_eq!("foo", var.name);
                    match expr.node {
                        ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
            assert!(parser.end());
        }

        #[test]
        fn if_statement() {
            let mut parser = Rdp::new(StringInput::new("if (2) 2;"));
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::If { ref cond, ref on_true, ref on_false } => {
                    match cond.node {
                        ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                        _ => assert!(false),
                    };
                    match on_true.node {
                        ast::Statement::Expression { ref expr } => {
                            match expr.node {
                                ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                                _ => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    };
                    assert!(on_false.is_none());
                }
                _ => assert!(false),
            };
            assert!(parser.end());

            let mut parser = Rdp::new(StringInput::new("if (2) 2; else 2;"));
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::If { ref cond, ref on_true, ref on_false } => {
                    match cond.node {
                        ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                        _ => assert!(false),
                    };
                    match on_true.node {
                        ast::Statement::Expression { ref expr } => {
                            match expr.node {
                                ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                                _ => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    };
                    match on_false.as_ref().unwrap().node {
                        ast::Statement::Expression { ref expr } => {
                            match expr.node {
                                ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                                _ => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    };
                }
                _ => assert!(false),
            };
            assert!(parser.end());
        }

        #[test]
        fn while_statement() {
            let mut parser = Rdp::new(StringInput::new("while (2) 2;"));
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::While { ref cond, ref body } => {
                    match cond.node {
                        ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                        _ => assert!(false),
                    };
                    match body.node {
                        ast::Statement::Expression { ref expr } => {
                            match expr.node {
                                ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                                _ => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    };
                }
                _ => assert!(false),
            };
            assert!(parser.end());
        }

        #[test]
        fn return_statement() {
            let mut parser = Rdp::new(StringInput::new("return; return 2;"));
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::Return { ref expr } => assert!(expr.is_none()),
                _ => assert!(false),
            };
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::Return { ref expr } => {
                    match expr.as_ref().unwrap().node {
                        ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            };
            assert!(parser.end());
        }

        #[test]
        fn compound() {
            let mut parser = Rdp::new(StringInput::new("{2;}"));
            parser.statement();
            match parser.parse_statement().node {
                ast::Statement::Compound { stmts } => {
                    assert_eq!(1, stmts.len());
                    match stmts[0].node {
                        ast::Statement::Expression { ref expr } => {
                            match expr.node {
                                ast::Expression::Literal { ref lit } => assert_eq!(&L, lit),
                                _ => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
            assert!(parser.end());
        }
    }
}
