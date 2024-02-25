use super::lex;
use crate::{ast_types::*, error::{NullError, NULL_ERROR}};
use nom::*;
use std::{mem, str};

// --------------- Some Helper Utils -------------------

type ParseResult<'a, T> = IResult<&'a [LexicalElement<'a>], T, NullError>;

fn escape_string_literal(a: &str) -> String {
    // TODO: make this actually escape
    a.to_string()
}

fn parse_number(a: &str) -> Result<f64, Err<NullError>> {
    //TODO: Make this handle hex literals
    a.parse::<f64>().map_err(|_| Err::Error(NULL_ERROR))
}

// TODO: Investigate returning incomplete?
fn parse_unit<'a>(
    expected: LexicalElement<'static>,
) -> impl FnMut(&'a [LexicalElement<'a>]) -> ParseResult<'a, &'a LexicalElement<'a>> {
    move |i: &'a [LexicalElement<'a>]| {
        if i.is_empty() {
            IResult::Err(Err::Error(NULL_ERROR))
        } else if i[0] != expected {
            IResult::Err(Err::Error(NULL_ERROR))
        } else {
            IResult::Ok((&i[1..], &i[0]))
        }
    }
}

/// Macro that takes a LexicalElement lifetime, a pattern to match against and an expression to
/// generate from the value and creates a parse that tries to match one element against that
/// pattern
macro_rules! destructure1 {
    ($l:lifetime, $p:pat => $d:expr) => {
        move |i: &$l [LexicalElement<$l>]| {
            if i.is_empty() {
                IResult::Err(Err::Error(NULL_ERROR))
            } else if let $p = i[0] {
                IResult::Ok((&i[1..], $d))
            } else {
                IResult::Err(Err::Error(NULL_ERROR))
            }
        }
    }
}

// ---------------- Actual parsers ---------------------

impl Parseable for UnOp {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::value(UnOp::Neg, parse_unit(LexicalElement::Minus)),
            combinator::value(UnOp::Not, parse_unit(LexicalElement::Keyword("not"))),
            combinator::value(UnOp::Len, parse_unit(LexicalElement::Hash)),
        ))(i)
    }
}

impl Parseable for BinOp {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::value(BinOp::Plus        , parse_unit(LexicalElement::Plus          )),
            combinator::value(BinOp::Minus       , parse_unit(LexicalElement::Minus         )),
            combinator::value(BinOp::Mult        , parse_unit(LexicalElement::Mult          )),
            combinator::value(BinOp::Div         , parse_unit(LexicalElement::Div           )),
            combinator::value(BinOp::Pow         , parse_unit(LexicalElement::Caret         )),
            combinator::value(BinOp::Mod         , parse_unit(LexicalElement::Mod           )),
            combinator::value(BinOp::Concat      , parse_unit(LexicalElement::Concat        )),
            combinator::value(BinOp::LessEqual   , parse_unit(LexicalElement::LessEqual     )),
            combinator::value(BinOp::LessThan    , parse_unit(LexicalElement::LessThan      )),
            combinator::value(BinOp::GreaterThan , parse_unit(LexicalElement::GreaterThan   )),
            combinator::value(BinOp::GreaterEqual, parse_unit(LexicalElement::GreaterEqual  )),
            combinator::value(BinOp::Equals      , parse_unit(LexicalElement::Equals        )),
            combinator::value(BinOp::NotEquals   , parse_unit(LexicalElement::NotEquals     )),
            combinator::value(BinOp::And         , parse_unit(LexicalElement::Keyword("and"))),
            combinator::value(BinOp::Or          , parse_unit(LexicalElement::Keyword("or") )),
        ))(i)
    }
}

impl Parseable for FieldSep {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::value(FieldSep::Semicolon, parse_unit(LexicalElement::Semicolon)),
            combinator::value(FieldSep::Comma, parse_unit(LexicalElement::Comma)),
        ))(i)
    }
}

impl Parseable for SimpleExp {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::value(SimpleExp::Elipsis, parse_unit(LexicalElement::Elipsis)),
            combinator::value(SimpleExp::Nil, parse_unit(LexicalElement::Keyword("nil"))),
            combinator::value(SimpleExp::False, parse_unit(LexicalElement::Keyword("false"))),
            combinator::value(SimpleExp::True, parse_unit(LexicalElement::Keyword("true"))),
            destructure1!('a, LexicalElement::StringLiteral(s) => SimpleExp::StringLiteral(escape_string_literal(s))),
            destructure1!('a, LexicalElement::Number(n) => SimpleExp::Number(parse_number(n)?)),
            combinator::map(TableConstructor::parse, |tc| SimpleExp::TableConstructor(Box::new(tc))),
            combinator::map(PrefixExp::parse, |px| SimpleExp::PrefixExp(Box::new(px))),
        ))(i)
    }
}

impl Parseable for Field {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::map(
                sequence::tuple((
                    destructure1!('a, LexicalElement::Identifier(i) => i),
                    parse_unit(LexicalElement::Assign),
                    Exp::parse,
                )),
                |(ident, _, exp)| Field::NamedExp(ident.to_string(), Box::new(exp)),
            ),
            combinator::map(Exp::parse, |exp| Field::Exp(Box::new(exp))),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::OpenSquare),
                    Exp::parse,
                    parse_unit(LexicalElement::CloseSquare),
                    parse_unit(LexicalElement::Assign),
                    Exp::parse,
                )),
                |(_, exp1, _, _, exp2)| Field::IndexExp(Box::new(exp1), Box::new(exp2)),
            )
        ))(i)
    }
}

impl Parseable for TableConstructor {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::tuple((
                parse_unit(LexicalElement::OpenBrace),
                combinator::opt(combinator::map(
                    sequence::tuple((
                        Field::parse,
                        multi::many0(
                            sequence::preceded(
                                FieldSep::parse,
                                Field::parse,
                            ),
                        ),
                    )),
                    |(s, mut r)| {
                        let mut v = vec![s];
                        v.append(&mut r);
                        v
                    },
                )),
                parse_unit(LexicalElement::CloseBrace),
            )),
            |(_, v, _)| TableConstructor(v.unwrap_or_else(|| vec![])),
        )(i)
    }
}

impl Parseable for NameList {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::tuple((
                destructure1!('a, LexicalElement::Identifier(a) => a.to_string()),
                multi::many0(sequence::preceded(
                    parse_unit(LexicalElement::Comma),
                    destructure1!('a, LexicalElement::Identifier(a) => a.to_string())
                )),
            )),
            |(s, mut r)| {
                let mut v = vec![s];
                v.append(&mut r);
                NameList(v)
            },
        )(i)
    }
}

impl Parseable for ExpList {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::tuple((
                Exp::parse,
                multi::many0(sequence::preceded(
                    parse_unit(LexicalElement::Comma),
                    Exp::parse
                )),
            )),
            |(s, mut r)| {
                let mut v = vec![s];
                v.append(&mut r);
                ExpList(v)
            },
        )(i)
    }
}

impl Parseable for Args {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::map(
                TableConstructor::parse,
                |tc| Args::TableConstructor(Box::new(tc)),
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::OpenParen),
                    parse_unit(LexicalElement::CloseParen),
                )),
                |_| Args::Empty,
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::OpenParen),
                    ExpList::parse,
                    parse_unit(LexicalElement::CloseParen),
                )),
                |(_, el, _)| Args::ExpList(Box::new(el)),
            ),
            destructure1!('a, LexicalElement::StringLiteral(a) => Args::StringLiteral(escape_string_literal(a))),
        ))(i)
    }
}

impl Parseable for ParList {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            destructure1!('a, LexicalElement::Elipsis => ParList(Vec::new(), true)),
            combinator::map(
                sequence::tuple((
                    NameList::parse,
                    combinator::opt(sequence::pair(
                        parse_unit(LexicalElement::Comma),
                        parse_unit(LexicalElement::Elipsis),
                    )),
                )),
                |(names, elipsis)| ParList(names.0, elipsis.is_some())
            ),
        ))(i)
    }
}

impl Parseable for FuncName {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::tuple((
                destructure1!('a, LexicalElement::Identifier(a) => a.to_string()),
                multi::many0(sequence::preceded(
                    parse_unit(LexicalElement::Dot),
                    destructure1!('a, LexicalElement::Identifier(a) => a.to_string()),
                )),
                combinator::opt(sequence::preceded(
                    parse_unit(LexicalElement::Colon),
                    destructure1!('a, LexicalElement::Identifier(a) => a.to_string()),
                ))
            )),
            |(a, mut r, l)| {
                let mut rv = vec![a];
                rv.append(&mut r);
                FuncName(rv, l)
            },
        )(i)
    }
}

impl Parseable for FuncBody {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::tuple((
                parse_unit(LexicalElement::OpenParen),
                combinator::opt(ParList::parse),
                parse_unit(LexicalElement::CloseParen),
                Chunk::parse,
                parse_unit(LexicalElement::Keyword("end")),
            )),
            |(_, pl, _, c, _)| {
                let pl = pl.unwrap_or_else(|| ParList(vec![], false));
                FuncBody(Box::new(pl), Box::new(c))
            },
        )(i)
    }
}

impl Parseable for Function {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::preceded(
                parse_unit(LexicalElement::Keyword("function")),
                FuncBody::parse,
            ),
            Function
        )(i)
    }
}

impl Parseable for NameAndArgs {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::pair(
                combinator::opt(sequence::preceded(
                    parse_unit(LexicalElement::Colon),
                    destructure1!('a, LexicalElement::Identifier(s) => s.to_string()),
                )),
                Args::parse,
            ),
            |(name, args)| NameAndArgs(name, args),
        )(i)
    }
}

impl Parseable for VarSuffix {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::map(
                sequence::tuple((
                    multi::many0(NameAndArgs::parse),
                    parse_unit(LexicalElement::OpenSquare),
                    Exp::parse,
                    parse_unit(LexicalElement::CloseSquare),
                )),
                |(naa, _, exp, _)| VarSuffix::Index(naa, exp),
            ),
            combinator::map(
                sequence::tuple((
                    multi::many0(NameAndArgs::parse),
                    parse_unit(LexicalElement::Dot),
                    destructure1!('a, LexicalElement::Identifier(s) => s.to_string()),
                )),
                |(naa, _, name)| VarSuffix::Member(naa, name),
            ),
        ))(i)
    }
}

impl Parseable for Var {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::map(
                sequence::pair(
                    destructure1!('a, LexicalElement::Identifier(s) => s.to_string()),
                    multi::many0(VarSuffix::parse),
                ),
                |(name, vss)| Var::Name(name, vss),
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::OpenParen),
                    Exp::parse,
                    parse_unit(LexicalElement::CloseParen),
                    multi::many1(VarSuffix::parse),
                )),
                |(_, exp, _, vss)| Var::Exp(exp, vss),
            ),
        ))(i)
    }
}

impl Parseable for VarList {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::pair(
                Var::parse,
                multi::many0(sequence::preceded(
                    parse_unit(LexicalElement::Comma),
                    Var::parse,
                )),
            ),
            |(var, mut rest)| {
                let mut v = vec![var];
                v.append(&mut rest);
                VarList(v)
            },
        )(i)
    }
}

// OMG I just realized how slow this would be, there is an exponential amount of attempts that the
// second option would fall back into the third
// FIXME: This should do the whole e0 ::= e1 (op e1)* thing, with construction done based on
// precedence. This is a holdover from my previous awful rebalancing strategy
impl Parseable for Exp {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::map(
                sequence::pair(
                    UnOp::parse,
                    Exp::parse,
                ),
                |(uo, ex)| Exp::UnaryOp(uo, Box::new(ex)),
            ),
            combinator::map(
                sequence::tuple((
                    SimpleExp::parse,
                    BinOp::parse,
                    Exp::parse,
                )),
                |(e1, bo, e2)| Exp::BinaryOp(Box::new(Exp::SimpleExp(Box::new(e1))), bo, Box::new(e2)),
            ),
            combinator::map(SimpleExp::parse, |se| Exp::SimpleExp(Box::new(se))),
        ))(i)
    }
}

impl Parseable for VarOrExp {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::map(Var::parse, |v| VarOrExp::Var(v)),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::OpenParen),
                    Exp::parse,
                    parse_unit(LexicalElement::CloseParen),
                )),
                |(_, e, _)| VarOrExp::Exp(e),
            ),
        ))(i)
    }
}

impl Parseable for PrefixExp {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::pair(
                VarOrExp::parse,
                multi::many0(NameAndArgs::parse),
            ),
            |(voe, naa)| PrefixExp(Box::new(voe), naa),
        )(i)
    }
}

impl Parseable for FunctionCall {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::pair(
                VarOrExp::parse,
                multi::many1(NameAndArgs::parse),
            ),
            |(voe, naa)| FunctionCall(Box::new(voe), naa),
        )(i)
    }
}

impl Parseable for Stat {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            combinator::map(
                sequence::tuple((
                    VarList::parse,
                    parse_unit(LexicalElement::Assign),
                    ExpList::parse,
                )),
                |(vl, _, el)| Stat::Assign(Box::new(vl), Box::new(el)),
            ),
            combinator::map(FunctionCall::parse, |fc| Stat::FunctionCall(Box::new(fc))),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("do")),
                    Chunk::parse,
                    parse_unit(LexicalElement::Keyword("end")),
                )),
                |(_, chunk, _)| Stat::DoBlock(Box::new(chunk)),
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("while")),
                    Exp::parse,
                    parse_unit(LexicalElement::Keyword("do")),
                    Chunk::parse,
                    parse_unit(LexicalElement::Keyword("end")),
                )),
                |(_, exp, _, chunk, _)| Stat::WhileBlock(Box::new(exp), Box::new(chunk)),
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("repeat")),
                    Chunk::parse,
                    parse_unit(LexicalElement::Keyword("until")),
                    Exp::parse,
                )),
                |(_, chunk, _, exp)| Stat::RepeatBlock(Box::new(exp), Box::new(chunk)),
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("if")),
                    Exp::parse,
                    parse_unit(LexicalElement::Keyword("then")),
                    Chunk::parse,
                    multi::many0(combinator::map(
                        sequence::tuple((
                            parse_unit(LexicalElement::Keyword("elseif")),
                            Exp::parse,
                            parse_unit(LexicalElement::Keyword("then")),
                            Chunk::parse,
                        )),
                        |(_, exp, _, chunk)| (exp, chunk),
                    )),
                    combinator::opt(
                        sequence::preceded(
                            parse_unit(LexicalElement::Keyword("else")),
                            Chunk::parse,
                        )
                    ),
                    parse_unit(LexicalElement::Keyword("end")),
                )),
                |(
                    _,
                    exp1,
                    _,
                    chunk1,
                    rest,
                    else_chunk,
                    _,
                )| {
                    let mut v = vec![(exp1, chunk1)];
                    let mut rest = rest;
                    v.append(&mut rest);
                    Stat::IfElseBlock(v, else_chunk.map(|x| Box::new(x)))
                },
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("for")),
                    destructure1!('a, LexicalElement::Identifier(s) => s.to_string()),
                    parse_unit(LexicalElement::Assign),
                    Exp::parse,
                    parse_unit(LexicalElement::Comma),
                    Exp::parse,
                    combinator::opt(sequence::preceded(parse_unit(LexicalElement::Comma), Exp::parse)),
                    parse_unit(LexicalElement::Keyword("do")),
                    Chunk::parse,
                    parse_unit(LexicalElement::Keyword("end")),
                )),
                |(_, name, _, e1, _, e2, e3, _, ch, _)| {
                    let mut ev = vec![e1, e2];
                    if let Some(v) = e3 {
                        ev.push(v);
                    }
                    Stat::ForRangeBlock(name, ev, Box::new(ch))
                },
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("for")),
                    NameList::parse,
                    parse_unit(LexicalElement::Keyword("in")),
                    ExpList::parse,
                    parse_unit(LexicalElement::Keyword("do")),
                    Chunk::parse,
                    parse_unit(LexicalElement::Keyword("end")),
                )),
                |(_, nl, _, el, _, ch, _)| Stat::ForInBlock(Box::new(nl), Box::new(el), Box::new(ch)),
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("function")),
                    FuncName::parse,
                    FuncBody::parse,
                )),
                |(_, name, body)| Stat::FunctionDec(name, Box::new(body)),
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("local")),
                    parse_unit(LexicalElement::Keyword("function")),
                    destructure1!('a, LexicalElement::Identifier(s) => s.to_string()),
                    FuncBody::parse,
                )),
                |(_, _, name, body)| Stat::LocalFunctionDec(name, Box::new(body)),
            ),
            combinator::map(
                sequence::tuple((
                    parse_unit(LexicalElement::Keyword("local")),
                    NameList::parse,
                    combinator::opt(sequence::preceded(
                        parse_unit(LexicalElement::Assign),
                        ExpList::parse,
                    )),
                )),
                |(_, nl, el)| {
                    let el = el.unwrap_or_else(|| ExpList(Vec::new()));
                    Stat::LocalAssign(Box::new(nl), el)
                },
            ),
        ))(i)
    }
}

impl Parseable for Chunk {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        combinator::map(
            sequence::pair(
                multi::many0(sequence::terminated(
                    Stat::parse,
                    combinator::opt(parse_unit(LexicalElement::Semicolon)),
                )),
                combinator::opt(sequence::terminated(
                    LastStat::parse,
                    combinator::opt(parse_unit(LexicalElement::Semicolon)),
                )),
            ),
            |(ss, ls)| Chunk(ss, ls),
        )(i)
    }
}

impl Parseable for LastStat {
    fn parse<'a>(i: &'a [LexicalElement<'a>]) -> IResult<&'a [LexicalElement<'a>], Self, NullError> {
        branch::alt((
            destructure1!('a, LexicalElement::Keyword("break") => LastStat::Break),
            combinator::map(
                sequence::preceded(
                    parse_unit(LexicalElement::Keyword("return")),
                    combinator::opt(ExpList::parse),
                ),
                |es| match es {
                    Some(es) => LastStat::Return(Box::new(es)),
                    None => LastStat::Return(Box::new(ExpList(Vec::new()))),
                },
            ),
        ))(i)
    }
}

#[derive(Default)]
struct ExpBalanceVisitor;

macro_rules! move_out {
    ($p:expr => $e:ident => $me:ident, $b:block) => {{
        let mut $me = mem::replace($e, $p);
        let r = $b;
        mem::swap(&mut $me, $e);
        r
    }};
}

use self::LeftOrRight::{Left, Right};
use std::cmp::Ord;
use std::cmp::Ordering::*;

impl ExpBalanceVisitor {
    fn move_unary_op_down(&mut self, e: Exp, uo: UnOp) -> Exp {
        match e {
            Exp::BinaryOp(e1, bo, e2) => {
                if UnOp::precedence() > bo.precedence() {
                    Exp::BinaryOp(Box::new(self.move_unary_op_down(*e1, uo)), bo, e2)
                } else {
                    Exp::UnaryOp(uo, Box::new(Exp::BinaryOp(e1, bo, e2)))
                }
            }
            e => Exp::UnaryOp(uo, Box::new(e)),
        }
    }
    fn descend_binary_op_on_right(&mut self, e: Exp, bo: BinOp, re: Exp) -> Exp {
        let mut recurse_right = |e, bo, re| match e {
            Exp::BinaryOp(ile, ibo, ire) => Exp::BinaryOp(
                ile,
                ibo,
                Box::new(self.descend_binary_op_on_right(*ire, bo, re)),
            ),
            Exp::UnaryOp(iuo, ire) => {
                Exp::UnaryOp(iuo, Box::new(self.descend_binary_op_on_right(*ire, bo, re)))
            }
            _ => unreachable!(),
        };
        match bo.precedence().cmp(&e.precedence()) {
            Greater => recurse_right(e, bo, re),
            Equal => {
                if bo.associativity() == Right {
                    recurse_right(e, bo, re)
                } else {
                    Exp::BinaryOp(Box::new(e), bo, Box::new(re))
                }
            }
            Less => Exp::BinaryOp(Box::new(e), bo, Box::new(re)),
        }
    }
    fn descend_binary_op_on_left(&mut self, e: Exp, bo: BinOp, le: Exp) -> Exp {
        let mut recurse_left = |e, bo, le| match e {
            Exp::BinaryOp(ile, ibo, ire) => Exp::BinaryOp(
                Box::new(self.descend_binary_op_on_left(*ile, bo, le)),
                ibo,
                ire,
            ),
            e @ Exp::UnaryOp(_, _) => Exp::BinaryOp(Box::new(le), bo, Box::new(e)),
            _ => unreachable!(),
        };
        match bo.precedence().cmp(&e.precedence()) {
            Greater => recurse_left(e, bo, le),
            Equal => {
                if bo.associativity() == Left {
                    recurse_left(e, bo, le)
                } else {
                    Exp::BinaryOp(Box::new(le), bo, Box::new(e))
                }
            }
            Less => Exp::BinaryOp(Box::new(le), bo, Box::new(e)),
        }
    }
    // This is gonna be awful from a performance perspective, but I had no idea
    // how else to do this
    fn destructure_left_subtree(
        &mut self,
        e: Exp,
        precedence: u8,
    ) -> (Exp, Box<dyn FnMut(Exp) -> Exp>) {
        match e {
            e @ Exp::SimpleExp(_) | e @ Exp::UnaryOp(_, _) => (e, Box::new(move |e: Exp| e)),
            Exp::BinaryOp(le, op, re) => {
                macro_rules! descend_left {
                    () => {{
                        let mut re = Some(re);
                        let (ie, mut ifn) = self.destructure_left_subtree(*le, precedence);
                        (
                            ie,
                            Box::new(move |pe: Exp| {
                                Exp::BinaryOp(Box::new(ifn(pe)), op, re.take().unwrap())
                            }),
                        )
                    }};
                }
                match op.precedence().cmp(&precedence) {
                    Less => descend_left!(),
                    Equal if op.associativity() == Left => descend_left!(),
                    _ => (Exp::BinaryOp(le, op, re), Box::new(move |e: Exp| e)),
                }
            }
        }
    }
    fn destructure_right_subtree(
        &mut self,
        e: Exp,
        precedence: u8,
    ) -> (Exp, Box<dyn FnMut(Exp) -> Exp>) {
        match e {
            e @ Exp::SimpleExp(_) => (e, Box::new(move |e: Exp| e)),
            Exp::UnaryOp(uo, ie) => match UnOp::precedence().cmp(&precedence) {
                Greater => {
                    let (ie, mut ifn) = self.destructure_right_subtree(*ie, precedence);
                    (
                        ie,
                        Box::new(move |pe: Exp| Exp::UnaryOp(uo, Box::new(ifn(pe)))),
                    )
                }
                Equal => unreachable!(),
                Less => (Exp::UnaryOp(uo, ie), Box::new(move |e: Exp| e)),
            },
            Exp::BinaryOp(le, op, re) => {
                macro_rules! descend_right {
                    () => {{
                        let mut le = Some(le);
                        let (ie, mut ifn) = self.destructure_right_subtree(*re, precedence);
                        (
                            ie,
                            Box::new(move |pe: Exp| {
                                Exp::BinaryOp(le.take().unwrap(), op, Box::new(ifn(pe)))
                            }),
                        )
                    }};
                }
                match op.precedence().cmp(&precedence) {
                    Less => descend_right!(),
                    Equal if op.associativity() == Right => descend_right!(),
                    _ => (Exp::BinaryOp(le, op, re), Box::new(move |e: Exp| e)),
                }
            }
        }
    }
}

impl ASTVisitor<u8> for ExpBalanceVisitor {
    fn visit_exp(&mut self, e: &mut Exp) -> Option<u8> {
        let default_exp = || Exp::SimpleExp(Box::new(SimpleExp::Nil));
        move_out!(default_exp() => e => me, {
            let ep = me.precedence();
            me = match me {
                Exp::SimpleExp(mut se) => {
                    self.visit_simple_exp(se.as_mut());
                    Exp::SimpleExp(se)
                },
                Exp::UnaryOp(op, ce) => {
                    self.move_unary_op_down(*ce, op)
                },
                Exp::BinaryOp(mut ce1, op, mut ce2) => {
                    let ce1p = self.visit_exp(ce1.as_mut()).unwrap();
                    let ce2p = self.visit_exp(ce2.as_mut()).unwrap();
                    let comparisons = (ep.cmp(&ce1p), ep.cmp(&ce2p));
                    if comparisons == (Greater, Greater) {
                        let bottom_side = match ce1p.cmp(&ce2p) {
                            Greater => {
                                Left
                            },
                            Equal => {
                                ce1.associativity().unwrap_or(Left)
                            },
                            Less => {
                                Right
                            },
                        };
                        let (ce1, mut lf) = self.destructure_right_subtree(*ce1, ep);
                        let (ce2, mut rf) =  self.destructure_left_subtree(*ce2, ep);
                        let bottom_exp = Exp::BinaryOp(Box::new(ce1), op, Box::new(ce2));
                        match bottom_side {
                            Left  => rf(lf(bottom_exp)),
                            Right => lf(rf(bottom_exp)),
                        }
                    } else {
                        let descend_dir = {
                            match comparisons {
                                (Greater, Greater) => unreachable!(),
                                (Equal, Greater) => Some(Right),
                                (Less, Greater) => Some(Right),
                                (Greater, Equal) => Some(Left),
                                (Equal, Equal) => Some(op.associativity().invert()),
                                (Less, Equal) => if op.associativity() == Left {
                                    Some(Right)
                                } else {
                                    None
                                },
                                (Greater, Less) => Some(Left),
                                (Equal, Less) => if op.associativity() == Right {
                                    Some(Left)
                                } else {
                                    None
                                },
                                (Less, Less) => None,
                            }
                        };
                        match descend_dir {
                            Some(Left) => {
                                self.descend_binary_op_on_right(*ce1, op, *ce2)
                            },
                            Some(Right) => {
                                self.descend_binary_op_on_left(*ce2, op, *ce1)
                            },
                            None => Exp::BinaryOp(ce1, op, ce2),
                        }
                    }
                },
            };
            Some(me.precedence())
        })
    }
}

#[allow(dead_code)]
pub fn parse_lua_source(input: &[u8]) -> Result<Chunk, NullError> {
    //let tokens = lex::tokenify_string(input).map_err(|_|ParseError::Error)?;
    let tokens = lex::tokenify_string(input)?;
    parse_chunk(&tokens)
}

#[allow(dead_code)]
pub fn parse_chunk(input: &[LexicalElement]) -> Result<Chunk, NullError> {
    let (_, mut parsed) = match Chunk::parse(input) {
        IResult::Ok((np, p)) => (np, p),
        _ => return Err(NULL_ERROR),
    };
    let mut ebv = ExpBalanceVisitor::default();
    ebv.visit_chunk(&mut parsed);
    Ok(parsed)
}

#[allow(dead_code)]
fn parse_exp(input: &[LexicalElement]) -> Result<Exp, NullError> {
    let (_, mut parsed) = match Exp::parse(input) {
        IResult::Ok((np, p)) => (np, p),
        _ => return Err(NULL_ERROR),
    };
    let mut ebv = ExpBalanceVisitor::default();
    ebv.visit_exp(&mut parsed);
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! tree {
        (+, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Plus, Box::new($b))
        };
        (-, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Minus, Box::new($b))
        };
        (*, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Mult, Box::new($b))
        };
        (/, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Div, Box::new($b))
        };
        (^, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Pow, Box::new($b))
        };
        (%, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Mod, Box::new($b))
        };
        (<, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::LessThan, Box::new($b))
        };
        (>, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::GreaterThan, Box::new($b))
        };
        (=, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Equals, Box::new($b))
        };
        (.., $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Concat, Box::new($b))
        };
        (<=, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::LessEqual, Box::new($b))
        };
        (>=, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::GreaterEqual, Box::new($b))
        };
        (!=, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::NotEquals, Box::new($b))
        };
        (&&, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::And, Box::new($b))
        };
        (||, $a:expr, $b:expr) => {
            Exp::BinaryOp(Box::new($a), BinOp::Or, Box::new($b))
        };
        (-, $e:expr) => {
            Exp::UnaryOp(UnOp::Neg, Box::new($e))
        };
        (!, $e:expr) => {
            Exp::UnaryOp(UnOp::Not, Box::new($e))
        };
        (#, $e:expr) => {
            Exp::UnaryOp(UnOp::Len, Box::new($e))
        };
        ($e:expr) => {
            Exp::SimpleExp(Box::new(SimpleExp::Number($e)))
        };
        () => {
            Exp::SimpleExp(Box::new(SimpleExp::Nil))
        };
    }

    fn fix_exp_tree(e: &mut Exp) {
        let mut ebv = ExpBalanceVisitor;
        ebv.visit_exp(e);
    }

    macro_rules! assert_transform {
        ($a:expr, $b:expr) => {{
            let mut a = $a;
            fix_exp_tree(&mut a);
            assert_eq!(a, $b);
        }};
    }

    #[test]
    fn test_exp_balancer() {
        //"nil"
        assert_transform!(tree!(), tree!());

        //"# 1.0 + 2.0"
        assert_transform!(
            tree!(
                #,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                )
            ),
            tree!(
                +,
                tree!(
                    #,
                    tree!(1.)
                ),
                tree!(2.)
            )
        );

        //"1.0 || 2.0 + 3.0"
        assert_transform!(
            tree!(
                +,
                tree!(
                    ||,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(3.)
            ),
            tree!(
                ||,
                tree!(1.),
                tree!(
                    +,
                    tree!(2.),
                    tree!(3.)
                )
            )
        );

        //"-1.0 ^ 2.0"
        assert_transform!(
            tree!(
                ^,
                tree!(
                    -,
                    tree!(1.)
                ),
                tree!(2.)
            ),
            tree!(
                -,
                tree!(
                    ^,
                    tree!(1.),
                    tree!(2.)
                )
            )
        );

        //"-1.0 + 2.0"
        assert_transform!(
            tree!(
                -,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                )
            ),
            tree!(
                +,
                tree!(
                    -,
                    tree!(1.)
                ),
                tree!(2.)
            )
        );

        //"1.0 + 2.0 - 3.0"
        assert_transform!(
            tree!(
                +,
                tree!(1.),
                tree!(
                    -,
                    tree!(2.),
                    tree!(3.)
                )
            ),
            tree!(
                -,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(3.)
            )
        );

        //"1.0 + 2.0 .. 3.0 .. 4.0"
        assert_transform!(
            tree!(
                ..,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(.., tree!(3.), tree!(4.))
            ),
            tree!(
                ..,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(.., tree!(3.), tree!(4.))
            )
        );

        //"1.0 .. 2.0 .. 3.0"
        assert_transform!(
            tree!(.., tree!(.., tree!(1.), tree!(2.)), tree!(3.)),
            tree!(.., tree!(1.), tree!(.., tree!(2.), tree!(3.)))
        );

        //"1.0 + 2.0 + 3.0"
        assert_transform!(
            tree!(
                +,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(3.)
            ),
            tree!(
                +,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(3.)
            )
        );

        //"1.0 + 2.0 + 3.0 + 4.0"
        assert_transform!(
            tree!(
                +,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(
                    +,
                    tree!(3.),
                    tree!(4.)
                )
            ),
            tree!(
                +,
                tree!(
                    +,
                    tree!(
                        +,
                        tree!(1.),
                        tree!(2.)
                    ),
                    tree!(3.)
                ),
                tree!(4.)
            )
        );

        //"1.0 .. 2.0 .. 3.0 .. 4.0"
        assert_transform!(
            tree!(
                ..,
                tree!(.., tree!(1.), tree!(2.)),
                tree!(.., tree!(3.), tree!(4.))
            ),
            tree!(
                ..,
                tree!(1.),
                tree!(.., tree!(2.), tree!(.., tree!(3.), tree!(4.)))
            )
        );

        //"- 1.0 ^ 2.0 ^ 3.)"
        assert_transform!(
            tree!(
                ^,
                tree!(
                    -,
                    tree!(1.)
                ),
                tree!(
                    ^,
                    tree!(2.),
                    tree!(3.)
                )
            ),
            tree!(
                -,
                tree!(
                    ^,
                    tree!(1.),
                    tree!(
                        ^,
                        tree!(2.),
                        tree!(3.)
                    )
                )
            )
        );

        //"1.0 + 2.0 ^ 3.0 + 4.0"
        assert_transform!(
            tree!(
                ^,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(
                    +,
                    tree!(3.),
                    tree!(4.)
                )
            ),
            tree!(
                +,
                tree!(
                    +,
                    tree!(1.),
                    tree!(
                        ^,
                        tree!(2.),
                        tree!(3.)
                    )
                ),
                tree!(4.)
            )
        );

        //"1.0 .. 2.0 ^ 3.0 .. 4.0"
        assert_transform!(
            tree!(
                ^,
                tree!(
                    ..,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(
                    ..,
                    tree!(3.),
                    tree!(4.)
                )
            ),
            tree!(
                ..,
                tree!(1.),
                tree!(
                    ..,
                    tree!(
                        ^,
                        tree!(2.),
                        tree!(3.)
                    ),
                    tree!(4.)
                )
            )
        );

        //"1.0 * 2.0 ^ 3.0 + 4.0"
        assert_transform!(
            tree!(
                ^,
                tree!(
                    *,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(
                    +,
                    tree!(3.),
                    tree!(4.)
                )
            ),
            tree!(
                +,
                tree!(
                    *,
                    tree!(1.),
                    tree!(
                        ^,
                        tree!(2.),
                        tree!(3.)
                    )
                ),
                tree!(4.)
            )
        );

        //"1.0 + 2.0 ^ 3.0 * 4.0"
        assert_transform!(
            tree!(
                ^,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(
                    *,
                    tree!(3.),
                    tree!(4.)
                )
            ),
            tree!(
                +,
                tree!(1.),
                tree!(
                    *,
                    tree!(
                        ^,
                        tree!(2.),
                        tree!(3.)
                    ),
                    tree!(4.)
                )
            )
        );
    }

    #[test]
    fn test_parse_table_constructor() {
        {
            let input = lex::tokenify_string(b"5").unwrap();
            let field: Field = Field::parse(&input).unwrap().1;
            assert_eq!(
                field,
                Field::Exp(Box::new(Exp::SimpleExp(Box::new(SimpleExp::Number(5.0)))))
            );
        }
        {
            let input = lex::tokenify_string(b"[5] = 5").unwrap();
            let field = Field::parse(&input).unwrap().1;
            assert_eq!(
                field,
                Field::IndexExp(
                    Box::new(Exp::SimpleExp(Box::new(SimpleExp::Number(5.0)))),
                    Box::new(Exp::SimpleExp(Box::new(SimpleExp::Number(5.0))))
                )
            );
        }
        {
            let input = lex::tokenify_string(b"a = 5").unwrap();
            let field = Field::parse(&input).unwrap().1;
            assert_eq!(
                field,
                Field::NamedExp(
                    "a".to_string(),
                    Box::new(Exp::SimpleExp(Box::new(SimpleExp::Number(5.0))))
                )
            );
        }
        {
            let input = lex::tokenify_string(b"local a = {a = 5}").unwrap();
            let chunk = parse_chunk(&input).unwrap();
            let expected = Chunk(
                vec![Stat::LocalAssign(
                    Box::new(NameList(vec![format!("a")])),
                    ExpList(vec![Exp::SimpleExp(Box::new(SimpleExp::TableConstructor(
                        Box::new(TableConstructor(vec![Field::NamedExp(
                            "a".to_string(),
                            Box::new(Exp::SimpleExp(Box::new(SimpleExp::Number(5.0)))),
                        )])),
                    )))]),
                )],
                None,
            );
            assert_eq!(chunk, expected);
        }
    }

    #[test]
    fn test_parser() {
        {
            let input = b"4.0";
            let input = lex::tokenify_string(&input[..]).unwrap();
            println!("{:?}", input);
            Exp::parse(&input).unwrap();
        }
        {
            let input = b"4.0 + - nil ^ nil";
            let input = lex::tokenify_string(&input[..]).unwrap();
            let (_, output) = Exp::parse(&input).unwrap();
            let expected = tree!(
                +,
                tree!(4.0),
                tree!(
                    -,
                    tree!(
                        ^,
                        tree!(),
                        tree!()
                    )
                )
            );
            assert_eq!(expected, output);
        }
        {
            let input = lex::tokenify_string(b"a = # 4 ^ 3").unwrap();
            {
                let (_, output) = Chunk::parse(&input).unwrap();
                println!("Before exp rotations: {:?}", output);
                let output = parse_chunk(&input).unwrap();
                println!("After exp rotations: {:?}", output);
            }
        }
        {
            let (_, exp) = Exp::parse(&lex::tokenify_string(b"4 + 4 ^ 4 + 4").unwrap()).unwrap();
            assert_eq!(
                exp,
                tree!(+, tree!(4.0), tree!(^, tree!(4.0),
                tree!(+, tree!(4.0), tree!(4.0))))
            );
        }
        //use std::mem::size_of;
        //println!("UnOp: {}"                      ,  size_of::<UnOp            >());
        //println!("BinOp: {}"                     ,  size_of::<BinOp           >());
        //println!("FieldSep: {}"                  ,  size_of::<FieldSep        >());
        //println!("SimpleExp: {}"                 ,  size_of::<SimpleExp       >());
        //println!("PrefixExp: {}"                 ,  size_of::<PrefixExp       >());
        //println!("Exp: {}"                       ,  size_of::<Exp             >());
        //println!("Field: {}"                     ,  size_of::<Field           >());
        //println!("TableConstructor: {}"          ,  size_of::<TableConstructor>());
        //println!("NameList: {}"                  ,  size_of::<NameList        >());
        //println!("ParList: {}"                   ,  size_of::<ParList         >());
        //println!("FuncName: {}"                  ,  size_of::<FuncName        >());
        //println!("ExpList: {}"                   ,  size_of::<ExpList         >());
        //println!("Args: {}"                      ,  size_of::<Args            >());
        //println!("Function: {}"                  ,  size_of::<Function        >());
        //println!("FuncBody: {}"                  ,  size_of::<FuncBody        >());
        //println!("FunctionCall: {}"              ,  size_of::<FunctionCall    >());
        //println!("Var: {}"                       ,  size_of::<Var             >());
        //println!("VarList: {}"                   ,  size_of::<VarList         >());
        //println!("Stat: {}"                      ,  size_of::<Stat            >());
        //println!("LastStat: {}"                  ,  size_of::<LastStat        >());
        //println!("Chunk: {}"                     ,  size_of::<Chunk           >());
        //println!("NameAndArgs: {}"               ,  size_of::<NameAndArgs     >());
        //println!("VarSuffix: {}"                 ,  size_of::<VarSuffix       >());
        //println!("VarOrExp: {}"                  ,  size_of::<VarOrExp        >());
    }

    #[test]
    fn test_parse_unit() {
        {
            let name = "hello";
            let lex = [LexicalElement::Minus, LexicalElement::Identifier(name)];
            assert!(parse_unit(LexicalElement::Plus)(&lex).is_err());
            assert_eq!(parse_unit(LexicalElement::Minus)(&lex).unwrap().1, &lex[0]);
        }
    }
}
