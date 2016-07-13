#[derive(PartialEq, Debug)]
pub enum TokenType {
    EQ,
    LT,

    PLUS,
    MINUS,
    ASTER,
    SLASH,
    AMP,

    Comma,
    SemiColon,

    LParenth,
    RParenth,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    KwFor,

    Identifier(String),

    INum(i32),
    FNum(f32),

    Str(String),

    EOF,
}
