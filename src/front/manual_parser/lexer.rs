use std::iter::Peekable;

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

pub struct Lexer<I: Iterator<Item = char>> {
    input: Peekable<I>,
    line: i32,
    col: i32,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    fn new(input: I) -> Lexer<I> {
        Lexer {
            input: input.peekable(),
            line: 0,
            col: 0,
        }
    }

    fn lex_word(&mut self, c: char) -> TokenType {
        let mut ident = self.input
            .by_ref()
            .take_while(|&c| c.is_alphabetic() || c.is_numeric() || c == '_')
            .collect::<String>();
        ident.insert(0, c);

        match ident.as_ref() {
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
            _ => TokenType::Identifier(ident),
        }
    }

    fn lex_number(&mut self, c: char) -> TokenType {
        let mut num = self.input
            .by_ref()
            .take_while(|&c| c.is_numeric() || c == '.')
            .collect::<String>();
        num.insert(0, c);

        if let Ok(n) = num.parse::<i32>() {
            TokenType::INum(n)
        } else if let Ok(n) = num.parse::<f32>() {
            TokenType::FNum(n)
        } else {
            panic!("wrong number format: {}", num);
        }
    }

    fn lex_string(&mut self) -> TokenType {
        TokenType::Str(self.input.by_ref().take_while(|&c| c != '\"').collect())
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = TokenType;

    fn next(&mut self) -> Option<TokenType> {
        loop {
            // handle EOF
            if self.input.peek().is_none() {
                return None;
            }

            return Some(match self.input.next().unwrap() {
                // handle WS
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
                    if let Some(&'=') = self.input.peek() {
                        self.input.next();
                        TokenType::EQ
                    } else {
                        TokenType::ASSIGN
                    }
                }
                '<' => {
                    if let Some(&'=') = self.input.peek() {
                        self.input.next();
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
            });
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn syntax() {
        let mut l = Lexer::new("! == < <= = + - * / , ; ( [ { } ] )".chars());
        assert_eq!(l.next(), Some(TokenType::NOT));
        assert_eq!(l.next(), Some(TokenType::EQ));
        assert_eq!(l.next(), Some(TokenType::LT));
        assert_eq!(l.next(), Some(TokenType::LE));
        assert_eq!(l.next(), Some(TokenType::ASSIGN));
        assert_eq!(l.next(), Some(TokenType::PLUS));
        assert_eq!(l.next(), Some(TokenType::MINUS));
        assert_eq!(l.next(), Some(TokenType::ASTER));
        assert_eq!(l.next(), Some(TokenType::SLASH));
        assert_eq!(l.next(), Some(TokenType::Comma));
        assert_eq!(l.next(), Some(TokenType::SemiColon));
        assert_eq!(l.next(), Some(TokenType::LParenth));
        assert_eq!(l.next(), Some(TokenType::LBracket));
        assert_eq!(l.next(), Some(TokenType::LBrace));
        assert_eq!(l.next(), Some(TokenType::RBrace));
        assert_eq!(l.next(), Some(TokenType::RBracket));
        assert_eq!(l.next(), Some(TokenType::RParenth));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn keywords() {
        let mut l = Lexer::new("void bool int float true false if else while return".chars());
        assert_eq!(l.next(), Some(TokenType::KwVoid));
        assert_eq!(l.next(), Some(TokenType::KwBool));
        assert_eq!(l.next(), Some(TokenType::KwInt));
        assert_eq!(l.next(), Some(TokenType::KwFloat));
        assert_eq!(l.next(), Some(TokenType::KwTrue));
        assert_eq!(l.next(), Some(TokenType::KwFalse));
        assert_eq!(l.next(), Some(TokenType::KwIf));
        assert_eq!(l.next(), Some(TokenType::KwElse));
        assert_eq!(l.next(), Some(TokenType::KwWhile));
        assert_eq!(l.next(), Some(TokenType::KwReturn));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn identifiers() {
        let mut l = Lexer::new("foo b4r ba_".chars());
        assert_eq!(l.next(), Some(TokenType::Identifier("foo".to_string())));
        assert_eq!(l.next(), Some(TokenType::Identifier("b4r".to_string())));
        assert_eq!(l.next(), Some(TokenType::Identifier("ba_".to_string())));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn numbers() {
        let mut l = Lexer::new("42 3.14".chars());
        assert_eq!(l.next(), Some(TokenType::INum(42)));
        assert_eq!(l.next(), Some(TokenType::FNum(3.14)));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn strings() {
        let mut l = Lexer::new("\"foo\" \"bar\"".chars());
        assert_eq!(l.next(), Some(TokenType::Str("foo".to_string())));
        assert_eq!(l.next(), Some(TokenType::Str("bar".to_string())));
        assert_eq!(l.next(), None);
    }
}
