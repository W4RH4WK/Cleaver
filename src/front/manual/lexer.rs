pub use super::token::TokenType;

pub struct Lexer<I: Iterator<Item = char>> {
    input: I,
    line: i32,
    col: i32,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    fn new(input: I) -> Lexer<I> {
        Lexer {
            input: input,
            line: 0,
            col: 0,
        }
    }

    fn lex(&mut self) -> TokenType {
        // TODO handle self.line and self.col
        loop {
            if let Some(c) = self.input.next() {
                return match c {
                    // handle WS
                    ' ' | '\t' | '\r' | '\n' => continue,

                    // handle symbols
                    '=' => TokenType::EQ,
                    '<' => TokenType::LT,
                    '+' => TokenType::PLUS,
                    '-' => TokenType::MINUS,
                    '*' => TokenType::ASTER,
                    '/' => TokenType::SLASH,
                    '&' => TokenType::AMP,
                    ',' => TokenType::Comma,
                    ';' => TokenType::SemiColon,

                    '(' => TokenType::LParenth,
                    ')' => TokenType::RParenth,
                    '[' => TokenType::LBracket,
                    ']' => TokenType::RBracket,
                    '{' => TokenType::LBrace,
                    '}' => TokenType::RBrace,

                    // handle keywords and identifier
                    'A' ... 'z' => self.lex_word(c),

                    // handle number
                    '0' ... '9' => self.lex_number(c),

                    // handle string
                    '\"' => self.lex_string(),

                    // catch all
                    x@_ => panic!("unexpected input: `{}`", x),
                };
            } else {
                return TokenType::EOF;
            }
        }
    }

    fn lex_word(&mut self, c: char) -> TokenType {
        let mut ident = self.input
            .by_ref()
            .take_while(|&c| c.is_alphabetic() || c.is_numeric() || c == '_')
            .collect::<String>();
        ident.insert(0, c);

        match ident.as_ref() {
            // handle keywords
            "for" => TokenType::KwFor,

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex() {
        let mut l = Lexer::new(",,".chars());
        assert_eq!(l.lex(), TokenType::Comma);
        assert_eq!(l.lex(), TokenType::Comma);
        assert_eq!(l.lex(), TokenType::EOF);
    }

    #[test]
    fn test_lex_word() {
        let mut l = Lexer::new("foo b4r ba_ for".chars());
        assert_eq!(l.lex(), TokenType::Identifier(String::from("foo")));
        assert_eq!(l.lex(), TokenType::Identifier(String::from("b4r")));
        assert_eq!(l.lex(), TokenType::Identifier(String::from("ba_")));
        assert_eq!(l.lex(), TokenType::KwFor);
    }

    #[test]
    fn test_lex_number() {
        let mut l = Lexer::new("42 3.14".chars());
        assert_eq!(l.lex(), TokenType::INum(42));
        assert_eq!(l.lex(), TokenType::FNum(3.14));
    }

    #[test]
    fn test_lex_string() {
        let mut l = Lexer::new("\"foo\" \"bar\"".chars());
        assert_eq!(l.lex(), TokenType::Str(String::from("foo")));
        assert_eq!(l.lex(), TokenType::Str(String::from("bar")));
    }
}
