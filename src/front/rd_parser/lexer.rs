use std::iter::Peekable;
use std::rc::Rc;

use front::Position;

#[derive(PartialEq, Debug)]
pub enum TokenType {
    NOT,
    EQ,
    LT,
    LE,

    ASSIGN,

    PLUS,
    MINUS,
    ASTER,
    SLASH,

    Comma,
    SemiColon,

    LParenth,
    RParenth,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    KwVoid,
    KwBool,
    KwInt,
    KwFloat,

    KwTrue,
    KwFalse,

    KwIf,
    KwElse,
    KwWhile,
    KwReturn,

    Identifier(String),

    INum(i32),
    FNum(f32),

    Str(String),
}

#[derive(PartialEq, Debug)]
pub struct Token {
    tok: TokenType,
    pos: Position,
}

pub struct Lexer<I: Iterator<Item = char>> {
    input: Peekable<I>,
    pos: Position,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    fn new(input: I) -> Lexer<I> {
        Lexer::new_named("stdin", input)
    }

    fn new_named(file: &str, input: I) -> Lexer<I> {
        Lexer {
            input: input.peekable(),
            pos: Position::new(Rc::new(file.to_string())),
        }
    }

    fn get_char(&mut self) -> Option<char> {
        self.input.next().map(|c| {
            self.update_position(c);
            c
        })
    }

    fn get_char_if<P>(&mut self, predicate: P) -> Option<char>
        where P: Fn(&char) -> bool
    {
        if self.input.peek().map(predicate).unwrap_or(false) {
            self.get_char()
        } else {
            None
        }
    }

    fn update_position(&mut self, c: char) {
        match c {
            '\n' => {
                self.pos.line += 1;
                self.pos.col = 1;
            }
            '\t' => self.pos.col += 7,
            _ => self.pos.col += 1,
        }
    }

    fn lex_word(&mut self, c: char) -> TokenType {
        let mut s = c.to_string();
        while let Some(c) = self.get_char_if(|&c| c.is_alphabetic() || c.is_numeric() || c == '_') {
            s.push(c);
        }

        match s.as_ref() {
            // handle type keywords
            "void" => TokenType::KwVoid,
            "bool" => TokenType::KwBool,
            "int" => TokenType::KwInt,
            "float" => TokenType::KwFloat,

            // handle bool literals
            "true" => TokenType::KwTrue,
            "false" => TokenType::KwFalse,

            // handle keywords
            "if" => TokenType::KwIf,
            "else" => TokenType::KwElse,
            "while" => TokenType::KwWhile,
            "return" => TokenType::KwReturn,

            // handle identifier
            _ => TokenType::Identifier(s),
        }
    }

    fn lex_number(&mut self, c: char) -> TokenType {
        let mut num = c.to_string();
        while let Some(n) = self.get_char_if(|&c| c.is_numeric() || c == '.') {
            num.push(n);
        }

        if let Ok(n) = num.parse::<i32>() {
            TokenType::INum(n)
        } else if let Ok(n) = num.parse::<f32>() {
            TokenType::FNum(n)
        } else {
            panic!("wrong number format: {}", num);
        }
    }

    fn lex_string(&mut self) -> TokenType {
        let mut s = "".to_string();
        while let Some(c) = self.get_char_if(|&c| c != '\"') {
            s.push(c);
        }

        // pop off \"
        if self.input.peek().is_some() {
            self.get_char();
        }

        TokenType::Str(s)
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            // handle EOF
            if self.input.peek().is_none() {
                return None;
            }

            let pos = self.pos.clone();

            let tok = match self.get_char().unwrap() {
                ' ' | '\t' | '\n' | '\r' => continue,

                // handle Syntax
                ',' => TokenType::Comma,
                ';' => TokenType::SemiColon,
                '(' => TokenType::LParenth,
                ')' => TokenType::RParenth,
                '[' => TokenType::LBracket,
                ']' => TokenType::RBracket,
                '{' => TokenType::LBrace,
                '}' => TokenType::RBrace,

                // handle Ops (simple)
                '!' => TokenType::NOT,
                '+' => TokenType::PLUS,
                '-' => TokenType::MINUS,
                '*' => TokenType::ASTER,
                '/' => TokenType::SLASH,

                // handle Ops (complex)
                '=' => {
                    if self.get_char_if(|&c| c == '=').is_some() {
                        TokenType::EQ
                    } else {
                        TokenType::ASSIGN
                    }
                }
                '<' => {
                    if self.get_char_if(|&c| c == '=').is_some() {
                        TokenType::LE
                    } else {
                        TokenType::LT
                    }
                }

                // handle keywords, types and identifier
                c @ 'A'...'z' => self.lex_word(c),

                // handle number
                c @ '0'...'9' => self.lex_number(c),

                // handle strings
                '\"' => self.lex_string(),

                x @ _ => panic!("unexpected input: `{}`", x),
            };

            return Some(Token {
                tok: tok,
                pos: pos,
            });
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn syntax() {
        let mut l = Lexer::new("! == < <= = + - * / , ; ([{}])".chars());
        assert_eq!(TokenType::NOT, l.next().unwrap().tok);
        assert_eq!(TokenType::EQ, l.next().unwrap().tok);
        assert_eq!(TokenType::LT, l.next().unwrap().tok);
        assert_eq!(TokenType::LE, l.next().unwrap().tok);
        assert_eq!(TokenType::ASSIGN, l.next().unwrap().tok);
        assert_eq!(TokenType::PLUS, l.next().unwrap().tok);
        assert_eq!(TokenType::MINUS, l.next().unwrap().tok);
        assert_eq!(TokenType::ASTER, l.next().unwrap().tok);
        assert_eq!(TokenType::SLASH, l.next().unwrap().tok);
        assert_eq!(TokenType::Comma, l.next().unwrap().tok);
        assert_eq!(TokenType::SemiColon, l.next().unwrap().tok);
        assert_eq!(TokenType::LParenth, l.next().unwrap().tok);
        assert_eq!(TokenType::LBracket, l.next().unwrap().tok);
        assert_eq!(TokenType::LBrace, l.next().unwrap().tok);
        assert_eq!(TokenType::RBrace, l.next().unwrap().tok);
        assert_eq!(TokenType::RBracket, l.next().unwrap().tok);
        assert_eq!(TokenType::RParenth, l.next().unwrap().tok);
        assert_eq!(None, l.next());
    }

    #[test]
    fn keywords() {
        let mut l = Lexer::new("void bool int float true false if else while,return".chars());
        assert_eq!(TokenType::KwVoid, l.next().unwrap().tok);
        assert_eq!(TokenType::KwBool, l.next().unwrap().tok);
        assert_eq!(TokenType::KwInt, l.next().unwrap().tok);
        assert_eq!(TokenType::KwFloat, l.next().unwrap().tok);
        assert_eq!(TokenType::KwTrue, l.next().unwrap().tok);
        assert_eq!(TokenType::KwFalse, l.next().unwrap().tok);
        assert_eq!(TokenType::KwIf, l.next().unwrap().tok);
        assert_eq!(TokenType::KwElse, l.next().unwrap().tok);
        assert_eq!(TokenType::KwWhile, l.next().unwrap().tok);
        assert_eq!(TokenType::Comma, l.next().unwrap().tok);
        assert_eq!(TokenType::KwReturn, l.next().unwrap().tok);
        assert_eq!(None, l.next());
    }

    #[test]
    fn identifiers() {
        let mut l = Lexer::new("foo b4r ba_".chars());
        assert_eq!(TokenType::Identifier("foo".to_string()),
                   l.next().unwrap().tok);
        assert_eq!(TokenType::Identifier("b4r".to_string()),
                   l.next().unwrap().tok);
        assert_eq!(TokenType::Identifier("ba_".to_string()),
                   l.next().unwrap().tok);
        assert_eq!(None, l.next());
    }

    #[test]
    fn numbers() {
        let mut l = Lexer::new("42 3.14".chars());
        assert_eq!(TokenType::INum(42), l.next().unwrap().tok);
        assert_eq!(TokenType::FNum(3.14), l.next().unwrap().tok);
        assert_eq!(None, l.next());
    }

    #[test]
    fn strings() {
        let mut l = Lexer::new("\"foo\" \"bar\"".chars());
        assert_eq!(TokenType::Str("foo".to_string()), l.next().unwrap().tok);
        assert_eq!(TokenType::Str("bar".to_string()), l.next().unwrap().tok);
        assert_eq!(None, l.next());
    }

    #[test]
    fn position() {
        // operator
        let mut l = Lexer::new("+ == -".chars());
        assert_eq!(1, l.next().unwrap().pos.col);
        assert_eq!(3, l.next().unwrap().pos.col);
        assert_eq!(6, l.next().unwrap().pos.col);

        // number
        l = Lexer::new("42 3.14".chars());
        assert_eq!(1, l.next().unwrap().pos.col);
        assert_eq!(4, l.next().unwrap().pos.col);

        // keyword
        l = Lexer::new("if while true".chars());
        assert_eq!(1, l.next().unwrap().pos.col);
        assert_eq!(4, l.next().unwrap().pos.col);
        assert_eq!(10, l.next().unwrap().pos.col);

        // strings
        l = Lexer::new("\"foo\" \"bar\"".chars());
        assert_eq!(1, l.next().unwrap().pos.col);
        assert_eq!(7, l.next().unwrap().pos.col);

        // lines
        l = Lexer::new("foo\n\"bar\nbaz\"\nbas".chars());
        assert_eq!((1, 1), l.next().unwrap().pos.to_pair());
        assert_eq!((2, 1), l.next().unwrap().pos.to_pair());
        assert_eq!((4, 1), l.next().unwrap().pos.to_pair());
    }
}
