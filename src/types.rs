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
pub enum FieldSep {
    Semicolon,
    Comma
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
    SimpleExp(Box<SimpleExp>),
    UnaryOp(UnOp, Box<Exp>),
    BinaryOp(Box<Exp>, BinOp, Box<Exp>),
    PrefixExp(Box<PrefixExp>),
    TableConstructor(Box<TableConstructor>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Field {
    Exp(Box<Exp>),
    NamedExp(String, Box<Exp>),
    IndexExp(Box<Exp>, Box<Exp>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct TableConstructor(pub Vec<Field>);

#[derive(Debug, Clone, PartialEq)]
pub struct NameList(pub Vec<String>);

#[derive(Debug, Clone, PartialEq)]
pub struct ParList(pub Vec<String>, pub bool);

#[derive(Debug, Clone, PartialEq)]
pub struct FuncName(pub Vec<String>, pub Option<String>);

#[derive(Debug, Clone, PartialEq)]
pub struct ExpList(pub Vec<Exp>);

#[derive(Debug, Clone, PartialEq)]
pub enum Args {
    ExpList(Box<ExpList>),
    TableConstructor(Box<TableConstructor>),
    StringLiteral(String)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function(pub FuncBody);

#[derive(Debug, Clone, PartialEq)]
pub struct FuncBody(pub Box<ParList>, pub Box<Chunk>);

#[derive(Debug, Clone, PartialEq)]
pub enum Stat{
}

#[derive(Debug, Clone, PartialEq)]
pub enum LastStat{
    Return(Box<ExpList>)
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(pub Vec<Stat>, pub Option<LastStat>);
