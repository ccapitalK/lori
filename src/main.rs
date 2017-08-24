#[macro_use]
extern crate nom;

use nom::*;
use std::fs::File;
use std::io::Read;
use std::str;

#[derive(Debug, Clone)]
enum LexicalElement<'a> {
    Keyword(&'static str),
    Identifier(&'a str),
    StringLiteral(&'a str),
    Number(&'a str),
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
    Assign,
    GreaterEqual,
    GreaterThan,
}

const KEYWORDS: [&str; 21] = [
    "and",
    "break",
    "do",
    "else",
    "elseif",
    "end",
    "false",
    "for",
    "function",
    "if",
    "in",
    "local",
    "nil",
    "not",
    "or",
    "repeat",
    "return",
    "then",
    "true",
    "until",
    "while",
];

//TODO: handle [===[ delimiters
fn parse_string_literal(input: &[u8]) -> IResult<&[u8], LexicalElement> {
    if input[0] != b'"' && input[0] != b'\'' {
        return IResult::Error(ErrorKind::IsNot);
    }
    let mut len = 1;
    while len < input.len() {
        if input[len] == input[0] {
            break;
        }else if len+1<input.len() && (input[len],input[len+1]) == (b'\\', input[0]) {
            len += 1;
        }
        len += 1;
    }
    if len < input.len() && input[len] == input[0] {
        let string = str::from_utf8(&input[1..len]).unwrap();
        IResult::Done(&input[len + 1..], LexicalElement::StringLiteral(string))
    } else {
        IResult::Error(ErrorKind::IsNot)
    }
}

fn parse_number(input: &[u8]) -> IResult<&[u8], LexicalElement> {
    //handle hex literal
    if input.len() >= 3 && &input[..2] == b"0x" {
        let (_, string) = try_parse!(&input[2..], is_a!("0123456789abcdefABCDEF"));
        let len = string.len();
        let num_string = str::from_utf8(&input[..len + 2]).unwrap();
        return IResult::Done(&input[len + 2..], LexicalElement::Number(num_string));
    }
    if input[0] == b'.' && !(input.len() > 1 && (input[1] as char).is_numeric()) {
        return IResult::Error(ErrorKind::IsNot);
    }
    let mut len = 0;
    while len < input.len() && (input[len] as char).is_numeric() {
        len+=1;
    }
    if len < input.len() && input[len] == b'.' {
        len+=1;
    }
    while len < input.len() && (input[len] as char).is_numeric() {
        len+=1;
    }
    if len == 0 {
        IResult::Error(ErrorKind::IsNot)
    } else {
        let num_string = str::from_utf8(&input[..len]).unwrap();
        IResult::Done(&input[len..], LexicalElement::Number(num_string))
    }
}

fn parse_identifier(input: &[u8]) -> IResult<&[u8], LexicalElement> {
    let is_valid_as_first =
        |x: u8| (b'a' <= x && x <= b'z') || (b'A' <= x && x <= b'Z') || x == b'_';
    let is_valid = |x: u8| (b'0' <= x && x <= b'9') || is_valid_as_first(x);
    if !is_valid_as_first(input[0]) {
        return IResult::Error(ErrorKind::IsNot);
    }
    let mut length = input.len();
    for i in 1..input.len() {
        if !is_valid(input[i]) {
            length = i;
            break;
        }
    }
    for word in KEYWORDS.iter() {
        if *word.as_bytes() == input[..length] {
            return IResult::Done(&input[length..], LexicalElement::Keyword(word));
        }
    }
    IResult::Done(
        &input[length..],
        LexicalElement::Identifier(str::from_utf8(&input[..length]).unwrap()),
    )
}

//TODO: handle comments
named!(parse_buffer<Vec<LexicalElement>>, ws!(many0!(alt!( 
       parse_identifier |
       parse_number |
       parse_string_literal |
       value!(LexicalElement::Comma       , tag!(","))  |
       value!(LexicalElement::Elipsis     , tag!("..."))|
       value!(LexicalElement::Concat      , tag!("..")) |
       value!(LexicalElement::Dot         , tag!("."))  |
       value!(LexicalElement::Plus        , tag!("+"))  |
       value!(LexicalElement::Minus       , tag!("-"))  |
       value!(LexicalElement::Mult        , tag!("*"))  |
       value!(LexicalElement::Div         , tag!("/"))  |
       value!(LexicalElement::Mod         , tag!("%"))  |
       value!(LexicalElement::Caret       , tag!("^"))  |
       value!(LexicalElement::Hash        , tag!("#"))  |
       value!(LexicalElement::OpenParen   , tag!("("))  |
       value!(LexicalElement::CloseParen  , tag!(")"))  |
       value!(LexicalElement::OpenBrace   , tag!("{"))  |
       value!(LexicalElement::CloseBrace  , tag!("}"))  |
       value!(LexicalElement::OpenSquare  , tag!("["))  |
       value!(LexicalElement::CloseSquare , tag!("]"))  |
       value!(LexicalElement::Semicolon   , tag!(";"))  |
       value!(LexicalElement::Colon       , tag!(":"))  |
       value!(LexicalElement::LessEqual   , tag!("<=")) |
       value!(LexicalElement::LessThan    , tag!("<"))  |
       value!(LexicalElement::Equals      , tag!("==")) |
       value!(LexicalElement::Assign      , tag!("="))  |
       value!(LexicalElement::GreaterEqual, tag!(">=")) |
       value!(LexicalElement::GreaterThan , tag!(">")) 
       ))));

fn main() {
    let mut data = Vec::new();
    {
        let mut f = File::open("in.lua").unwrap();
        f.read_to_end(&mut data).unwrap();
    }
    println!("{:?}", parse_buffer(&data[..]));
}
