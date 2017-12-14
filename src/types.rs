#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexicalElement<'a> {
    Keyword(&'static str),
    Identifier(&'a str),
    StringLiteral(&'a str),
    Number(&'a str),
    Comment,
    Comma,
    Dot,
    Concat,
    Elipsis,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    Caret,
    Hash,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenBrace,
    CloseBrace,
    Colon,
    Semicolon,
    LessEqual,
    LessThan,
    Equals,
    NotEquals,
    Assign,
    GreaterEqual,
    GreaterThan,
}

use nom::IResult;

pub trait Parseable : Sized {
    fn parse<'a, 'b>(i: &'a[LexicalElement<'b>]) 
        -> IResult<&'a[LexicalElement<'b>], Self>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
    Len
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Mult,
    Div,
    Pow,
    Mod,
    Concat,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equals,
    NotEquals,
    And,
    Or
}

impl BinOp {
    fn precedence(&self) -> u8 {
        //TODO: Finish this
        match *self {
            BinOp::Plus => 0,
            _ => 1
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleExp {
    Nil,
    False,
    True,
    Number(f64),
    StringLiteral(String),
    Elipsis,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixExp {
    BracketExp(Exp)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    SimpleExp(SimpleExp),
    UnaryOp(UnOp, Box<Exp>),
    BinaryOp(Box<Exp>, BinOp, Box<Exp>),
    PrefixExp(Box<PrefixExp>)
}

struct Chunk;
