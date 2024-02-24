use crate::ast_types::LexicalElement;
use crate::error::*;
use nom::bytes::complete::{is_a, is_not, tag};
use nom::combinator::value;
use nom::*;
use std::str;

pub type LexResult<'a> = IResult<&'a [u8], LexicalElement<'a>, NullError>;

const KEYWORDS: [&str; 21] = [
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local",
    "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
];

fn parse_multiline_string(input: &[u8]) -> LexResult {
    let mut i = 0;
    {
        if input.len() < 2 || input[i] != b'[' {
            return IResult::Err(Err::Error(NULL_ERROR));
        }
        i += 1;
        while i < input.len() && input[i] == b'=' {
            i += 1;
        }
        if input.len() <= i || input[i] != b'[' {
            return IResult::Err(Err::Error(NULL_ERROR));
        }
        i += 1;
    }
    let len = i - 2;
    {
        while i < input.len() - (len + 2) {
            if input[i] == b']'
                && input[i + len + 1] == b']'
                && input[i + 1..i + len + 1]
                    .iter()
                    .filter(|&x| x != &b'=')
                    .count()
                    == 0
            {
                let out_range = &input[len + 2..i];
                let out_token = LexicalElement::StringLiteral(str::from_utf8(out_range).unwrap());
                return IResult::Ok((&input[i + len + 2..], out_token));
            }
            i += 1;
        }
    }
    // TODO: EOF
    IResult::Err(Err::Incomplete(Needed::Unknown))
}

fn parse_string_literal(input: &[u8]) -> LexResult {
    if input.is_empty() || input[0] != b'"' && input[0] != b'\'' {
        return IResult::Err(Err::Error(NULL_ERROR));
    }
    let mut len = 1;
    while len < input.len() {
        if input[len] == input[0] {
            break;
        } else if len + 1 < input.len() && (input[len], input[len + 1]) == (b'\\', input[0]) {
            len += 1;
        }
        len += 1;
    }
    if len < input.len() && input[len] == input[0] {
        let string = str::from_utf8(&input[1..len]).unwrap();
        IResult::Ok((&input[len + 1..], LexicalElement::StringLiteral(string)))
    } else {
        IResult::Err(Err::Error(NULL_ERROR))
    }
}

fn parse_number(input: &[u8]) -> LexResult {
    //handle hex literal
    if input.len() >= 3 && &input[..2] == b"0x" {
        let (_, string) = is_a("0123456789abcdefABCDEF")(&input[2..])?;
        let len = string.len();
        let num_string = str::from_utf8(&input[..len + 2]).unwrap();
        return IResult::Ok((&input[len + 2..], LexicalElement::Number(num_string)));
    }
    if input.is_empty() || input[0] == b'.' && !(input.len() > 1 && (input[1] as char).is_numeric())
    {
        return IResult::Err(Err::Error(NULL_ERROR));
    }
    let mut len = 0;
    while len < input.len() && (input[len] as char).is_numeric() {
        len += 1;
    }
    if len < input.len() && input[len] == b'.' {
        len += 1;
    }
    while len < input.len() && (input[len] as char).is_numeric() {
        len += 1;
    }
    if len == 0 {
        IResult::Err(Err::Error(NULL_ERROR))
    } else {
        let num_string = str::from_utf8(&input[..len]).unwrap();
        IResult::Ok((&input[len..], LexicalElement::Number(num_string)))
    }
}

fn parse_identifier(input: &[u8]) -> LexResult {
    let is_valid_as_first =
        |x: u8| (b'a' <= x && x <= b'z') || (b'A' <= x && x <= b'Z') || x == b'_';
    let is_valid = |x: u8| (b'0' <= x && x <= b'9') || is_valid_as_first(x);
    if input.is_empty() || !is_valid_as_first(input[0]) {
        return IResult::Err(Err::Error(NULL_ERROR));
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
            return IResult::Ok((&input[length..], LexicalElement::Keyword(word)));
        }
    }
    IResult::Ok((
        &input[length..],
        LexicalElement::Identifier(str::from_utf8(&input[..length]).unwrap()),
    ))
}

fn parse_comment(i: &[u8]) -> LexResult {
    let (i, _) = tag("--")(i)?;
    let mut alt_rest = branch::alt((
        // FIXME: This isn't quite a multiline string
        value(LexicalElement::Comment, parse_multiline_string),
        value(LexicalElement::Comment, combinator::opt(is_not("\n"))),
    ));
    alt_rest(i)
}

fn skip_whitespace(i: &[u8]) -> IResult<&[u8], (), NullError> {
    value((), combinator::opt(is_a(" \t\n\r")))(i)
}

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
#[rustfmt::skip]
pub fn lex_one(i: &[u8]) -> LexResult {
    let (i, _) = skip_whitespace(i)?;
    // Lol I guess it's not unrolled far enough
    let constant_comb1 = branch::alt((
        value(LexicalElement::Comma       , tag(",")  ),
        value(LexicalElement::Elipsis     , tag("...")),
        value(LexicalElement::Concat      , tag("..") ),
        value(LexicalElement::Dot         , tag(".")  ),
        value(LexicalElement::Plus        , tag("+")  ),
        value(LexicalElement::Minus       , tag("-")  ),
        value(LexicalElement::Mult        , tag("*")  ),
        value(LexicalElement::Div         , tag("/")  ),
        value(LexicalElement::Mod         , tag("%")  ),
        value(LexicalElement::Caret       , tag("^")  ),
        value(LexicalElement::Hash        , tag("#")  ),
        value(LexicalElement::OpenParen   , tag("(")  ),
        value(LexicalElement::CloseParen  , tag(")")  ),
    ));
    let constant_comb2 = branch::alt((
        value(LexicalElement::OpenBrace   , tag("{")  ),
        value(LexicalElement::CloseBrace  , tag("}")  ),
        value(LexicalElement::OpenSquare  , tag("[")  ),
        value(LexicalElement::CloseSquare , tag("]")  ),
        value(LexicalElement::Semicolon   , tag(";")  ),
        value(LexicalElement::Colon       , tag(":")  ),
        value(LexicalElement::LessEqual   , tag("<=") ),
        value(LexicalElement::LessThan    , tag("<")  ),
        value(LexicalElement::Equals      , tag("==") ),
        value(LexicalElement::NotEquals   , tag("~=") ),
        value(LexicalElement::Assign      , tag("=")  ),
        value(LexicalElement::GreaterEqual, tag(">=") ),
        value(LexicalElement::GreaterThan , tag(">")  ),
    ));
    let mut comb = branch::alt((
        parse_identifier,
        parse_number,
        parse_comment,
        parse_string_literal,
        parse_multiline_string,
        constant_comb1,
        constant_comb2,
    ));
    comb(i)
}

/// Get the raw lexemes from a buffer
pub fn lex_buffer(i: &[u8]) -> Result<Vec<LexicalElement>, Err<NullError>> {
    let (i, c) = multi::many0(lex_one)(i)?;
    let (i, _) = skip_whitespace(i)?;
    if !i.is_empty() {
        return Err(Err::Incomplete(Needed::Unknown));
    }
    Ok(c)
}

/// Lex a buffer, and then lift the lexemes into higher level lexemes
// TODO: Convert lexed StringLiterals to their escaped ref-counted equivalents
pub fn tokenify_string(data: &[u8]) -> Result<Vec<LexicalElement>, NullError> {
    let matched: Vec<_> = lex_buffer(data).map_err(|_e| NULL_ERROR)?;
    Ok(matched
        .into_iter()
        .filter(|ref mut x| **x != LexicalElement::Comment)
        .collect())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex_one() {
        let data = b" ,+";
        let (data, lexed) = lex_one(data).unwrap();
        assert_eq!(lexed, LexicalElement::Comma);
        let (_, lexed) = lex_one(data).unwrap();
        assert_eq!(lexed, LexicalElement::Plus);
    }

    #[test]
    fn test_lexer() {
        use std::cmp::Ordering;
        use std::fs::File;
        use std::io::Read;
        let mut data = Vec::new();
        {
            let mut f = File::open("tests/lex/test1").unwrap();
            f.read_to_end(&mut data).unwrap();
        }
        let tokens = tokenify_string(data.as_slice()).unwrap();
        let expected = [
            LexicalElement::Comma,
            LexicalElement::Elipsis,
            LexicalElement::Concat,
            LexicalElement::Dot,
            LexicalElement::Plus,
            LexicalElement::Minus,
            LexicalElement::Mult,
            LexicalElement::Div,
            LexicalElement::Mod,
            LexicalElement::Caret,
            LexicalElement::Hash,
            LexicalElement::OpenParen,
            LexicalElement::CloseParen,
            LexicalElement::OpenBrace,
            LexicalElement::CloseBrace,
            LexicalElement::OpenSquare,
            LexicalElement::CloseSquare,
            LexicalElement::Semicolon,
            LexicalElement::Colon,
            LexicalElement::LessEqual,
            LexicalElement::LessThan,
            LexicalElement::Equals,
            LexicalElement::Assign,
            LexicalElement::GreaterEqual,
            LexicalElement::GreaterThan,
            LexicalElement::Keyword("and"),
            LexicalElement::Keyword("break"),
            LexicalElement::Keyword("do"),
            LexicalElement::Keyword("else"),
            LexicalElement::Keyword("elseif"),
            LexicalElement::Keyword("end"),
            LexicalElement::Keyword("false"),
            LexicalElement::Keyword("for"),
            LexicalElement::Keyword("function"),
            LexicalElement::Keyword("if"),
            LexicalElement::Keyword("in"),
            LexicalElement::Keyword("local"),
            LexicalElement::Keyword("nil"),
            LexicalElement::Keyword("not"),
            LexicalElement::Keyword("or"),
            LexicalElement::Keyword("repeat"),
            LexicalElement::Keyword("return"),
            LexicalElement::Keyword("then"),
            LexicalElement::Keyword("true"),
            LexicalElement::Keyword("until"),
            LexicalElement::Keyword("while"),
            LexicalElement::Identifier("hello"),
            LexicalElement::Identifier("waldo7"),
            LexicalElement::Number("7"),
            LexicalElement::Identifier("wait"),
            LexicalElement::Number("8.0"),
            LexicalElement::StringLiteral("Hello"),
        ];
        for (t, e) in tokens.iter().zip(expected.iter()) {
            if t != e {
                panic!("Expected {:?}, got {:?}", e, t);
            }
        }
        match tokens.len().cmp(&expected.len()) {
            Ordering::Less => {
                panic!("Unexpected EOF: Expected {:?}", expected[tokens.len()]);
            }
            Ordering::Greater => {
                panic!("Expected EOF: Got {:?}", tokens[expected.len()]);
            }
            _ => {}
        }
    }
}
