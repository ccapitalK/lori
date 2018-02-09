use nom::*;
use std::str;
use ast_types::LexicalElement;

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

fn parse_multiline_string(input: &[u8]) -> IResult<&[u8], LexicalElement> {
    let mut i = 0;
    {
        if input.len() <  2 || input[i] != b'[' {
            return IResult::Error(ErrorKind::IsNot);
        }
        i += 1;
        while i < input.len() && input[i] == b'=' {
            i += 1;
        }
        if input.len() <= i || input[i] != b'[' {
            return IResult::Error(ErrorKind::IsNot);
        }
        i += 1;
    }
    let len = i - 2;
    {
        while i < input.len() - (len + 2) {
            if input[i] == b']' && input[i+len+1] == b']' && 
                input[i+1..i+len+1].iter().filter(|&x| x != &b'=').count() == 0 {
                let out_range = &input[len+2..i];
                let out_token = LexicalElement::StringLiteral(str::from_utf8(out_range).unwrap());
                return IResult::Done(&input[i+len+2..], out_token);
            }
            i += 1;
        }
    }
    IResult::Incomplete(Needed::Unknown)
}

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

named!(parse_comment<LexicalElement>, do_parse!(
        tag!("--") >>
        alt!(
            call!(parse_multiline_string) |
            do_parse!(
                opt!(is_not!("\n")) >>
                e: alt!(
                    eof!() |
                    tag!("\n")
                ) >>
                (LexicalElement::Comment)
            )
        ) >>
        (LexicalElement::Comment)
    ));

//  TODO: handle edge cases in string literals
//  - conversion between string data and actual escaped strings
//  - if first character in multiline is newline, don't add this multiline
//  TODO: URGENT: handle escapes in string literals
//  TODO: Identifiers can contain any characters considered 
//      'alphabetic in the current locale'
//  TODO: Exponential notation
//  TODO: hexadecimal double notation (dear lord, why?)
//  TODO: You know what, redo this whole thing,
//      a bunch of stuff needs to be reconsidered from an 
//      architectural perspective anyway (how would error messages be implemented?
named!(pub lex_buffer<Vec<LexicalElement>>, ws!(many0!(alt!( 
       parse_identifier |
       parse_number |
       parse_comment |
       parse_string_literal |
       parse_multiline_string |
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
       value!(LexicalElement::NotEquals   , tag!("~=")) |
       value!(LexicalElement::Assign      , tag!("="))  |
       value!(LexicalElement::GreaterEqual, tag!(">=")) |
       value!(LexicalElement::GreaterThan , tag!(">")) 
       ))));

pub fn tokenify_string(data: &[u8]) -> Result<Vec<LexicalElement>,()> {
    match lex_buffer(data) {
        IResult::Done(_, v) => Ok(v.into_iter().filter(|ref mut x| **x != LexicalElement::Comment)
                                  .collect()),
        _ => Err(()),
    }
}

#[test]
fn test_lexer() {
    use std::fs::File;
    use std::io::Read;
    use std::cmp::Ordering;
    use ast_types::LexicalElement::*;
    let mut data = Vec::new();
    {
        let mut f = File::open("tests/lex/test1").unwrap();
        f.read_to_end(&mut data).unwrap();
    }
    let tokens = tokenify_string(data.as_slice()).unwrap();
    let expected = [Comma, Elipsis, Concat, Dot, Plus, Minus, Mult, Div, 
        Mod, Caret, Hash, OpenParen, CloseParen, OpenBrace, CloseBrace,
        OpenSquare, CloseSquare, Semicolon, Colon, LessEqual, LessThan,
        Equals, Assign, GreaterEqual, GreaterThan, Keyword("and"), 
        Keyword("break"), Keyword("do"), Keyword("else"), Keyword("elseif"),
        Keyword("end"), Keyword("false"), Keyword("for"), Keyword("function"),
        Keyword("if"), Keyword("in"), Keyword("local"), Keyword("nil"),
        Keyword("not"), Keyword("or"), Keyword("repeat"), Keyword("return"),
        Keyword("then"), Keyword("true"), Keyword("until"), Keyword("while"),
        Identifier("hello"), Identifier("waldo7"), Number("7"), 
        Identifier("wait"), Number("8.0"), StringLiteral("Hello")
    ];

    match tokens.len().cmp(&expected.len()) {
        Ordering::Less => {
            panic!("Unexpected EOF: Expected {:?}", expected[tokens.len()]);
        },
        Ordering::Greater => {
            panic!("Expected EOF: Got {:?}", tokens[expected.len()]);
        },
        Ordering::Equal => for (t, e) in tokens.iter().zip(expected.iter()) {
            if t != e {
                panic!("Expected {:?}, got {:?}", e, t);
            }
        }
    }
}
