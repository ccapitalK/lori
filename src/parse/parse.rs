use nom::*;
use std::{str, mem};
use types::*;
use super::lex;

// --------------- Some Helper Macros ------------------

// implement the parseable trait for a type, using a interface
// similar to named!
macro_rules! impl_parse {
    ($t:ty, $input:ident, $d:block) => (
        impl Parseable for $t {
            fn parse<'a, 'b>($input: &'a[LexicalElement<'b>]) 
                -> IResult<&'a[LexicalElement<'b>], Self>{
                $d
            }
        }
    );
    ($t:ty, $submac:ident!( $($args:tt)* )) => (
        impl Parseable for $t {
            fn parse<'a, 'b>(i: &'a[LexicalElement<'b>]) 
                -> IResult<&'a[LexicalElement<'b>], Self>{
                    $submac!(i, $($args)*)
            }
        }
    );
}

macro_rules! le_tag {
    ($i:expr, $inp:pat) => ({
        if $i.len() < 1 {
            IResult::Error(ErrorKind::Custom(0))
        } else if let $inp = $i[0] {
            IResult::Done(&$i[1..], $i[0].clone())
        } else {
            IResult::Error(ErrorKind::Tag)
        }
    });
    ($i:expr, $inp:pat => $r:expr) => ({
        if $i.len() < 1 {
            IResult::Error(ErrorKind::Custom(0))
        } else if let $inp = $i[0] {
            IResult::Done(&$i[1..], $r)
        } else {
            IResult::Error(ErrorKind::Tag)
        }
    });
}

// ---------------- Actual parsers ---------------------

impl_parse!(UnOp, alt!(
        le_tag!(LexicalElement::Minus => UnOp::Neg) |
        le_tag!(LexicalElement::Keyword("not") => UnOp::Not) |
        le_tag!(LexicalElement::Hash => UnOp::Len)
    ));

impl_parse!(BinOp, alt!(
        le_tag!(LexicalElement::Plus           => BinOp::Plus        )  |         
        le_tag!(LexicalElement::Minus          => BinOp::Minus       )  |          
        le_tag!(LexicalElement::Mult           => BinOp::Mult        )  |         
        le_tag!(LexicalElement::Div            => BinOp::Div         )  |        
        le_tag!(LexicalElement::Caret          => BinOp::Pow         )  |        
        le_tag!(LexicalElement::Mod            => BinOp::Mod         )  |        
        le_tag!(LexicalElement::Concat         => BinOp::Concat      )  |           
        le_tag!(LexicalElement::LessEqual      => BinOp::LessEqual   )  |              
        le_tag!(LexicalElement::LessThan       => BinOp::LessThan    )  |             
        le_tag!(LexicalElement::GreaterThan    => BinOp::GreaterThan )  |                
        le_tag!(LexicalElement::GreaterEqual   => BinOp::GreaterEqual)  |                 
        le_tag!(LexicalElement::Equals         => BinOp::Equals      )  |           
        le_tag!(LexicalElement::NotEquals      => BinOp::NotEquals   )  |              
        le_tag!(LexicalElement::Keyword("and") => BinOp::And         )  |        
        le_tag!(LexicalElement::Keyword("or")  => BinOp::Or          )          
    ));

impl_parse!(FieldSep, alt!(
        le_tag!(LexicalElement::Semicolon => FieldSep::Semicolon)       |
        le_tag!(LexicalElement::Comma => FieldSep::Comma) 
    ));

fn escape_string_literal(a: &str) -> String {
    // TODO: make this actually escape
    a.to_string()
}

fn parse_number(a: &str) -> f64 {
    //TODO: Make this handle hex literals
    a.parse::<f64>().unwrap()
}

impl_parse!(SimpleExp, alt!(
        le_tag!(LexicalElement::Keyword("nil") => SimpleExp::Nil)     |
        le_tag!(LexicalElement::Keyword("false") => SimpleExp::False) |
        le_tag!(LexicalElement::Keyword("true") => SimpleExp::True)   |
        le_tag!(LexicalElement::Elipsis => SimpleExp::Elipsis)        |
        le_tag!(LexicalElement::StringLiteral(a) => 
                SimpleExp::StringLiteral(escape_string_literal(a)))   |
        le_tag!(LexicalElement::Number(a) => 
                SimpleExp::Number(parse_number(a)))                   |
        do_parse!(tc: call!(TableConstructor::parse) >> 
                  (SimpleExp::TableConstructor(Box::new(tc))))        |
        do_parse!(px: call!(PrefixExp::parse) >> 
                  (SimpleExp::PrefixExp(Box::new(px))))
    ));

impl_parse!(Field, alt!(
        do_parse!(v: call!(Exp::parse) >> (Field::Exp(Box::new(v)))) |
        do_parse!(n: le_tag!(LexicalElement::Identifier(s) => s) >> 
                  le_tag!(LexicalElement::Assign) >>
                  v: call!(Exp::parse) >> 
                  (Field::NamedExp(n.to_string(), Box::new(v)))) |
        do_parse!(le_tag!(LexicalElement::OpenSquare) >>
                  v1: call!(Exp::parse) >> 
                  le_tag!(LexicalElement::CloseSquare) >>
                  le_tag!(LexicalElement::Assign) >>
                  v2: call!(Exp::parse) >> 
                  (Field::IndexExp(Box::new(v1), Box::new(v2))))
    ));

impl_parse!(TableConstructor, do_parse!(
        le_tag!(LexicalElement::OpenBrace) >>
        r: opt!(
            do_parse!(
                h: call!(Field::parse) >>
                r: many0!(
                    do_parse!(
                        call!(FieldSep::parse) >>
                        h: call!(Field::parse) >>
                        (h)
                    )
                ) >>
                opt!(call!(FieldSep::parse)) >>
                ({
                    let mut rv = Vec::new();
                    let mut r = r;
                    rv.push(h);
                    rv.append(&mut r);
                    rv
                })
            )
        ) >>
        opt!(call!(FieldSep::parse)) >>
        le_tag!(LexicalElement::CloseBrace) >>
        (TableConstructor(match r {
            Some(v) => v,
            None => Vec::new()
        }))
    ));

impl_parse!(NameList, do_parse!(
        name: le_tag!(LexicalElement::Identifier(a) => a.to_string()) >>
        rest: many0!(
            do_parse!(
                le_tag!(LexicalElement::Comma) >>
                name: le_tag!(LexicalElement::Identifier(a) => a.to_string()) >>
                (name)
            )
        ) >>
        ({
            let mut rv = Vec::new();
            let mut rest = rest;
            rv.push(name);
            rv.append(&mut rest);
            NameList(rv)
        })
    ));

impl_parse!(ExpList, do_parse!(
        exp:  call!(Exp::parse) >>
        rest: many0!(
            do_parse!(
                le_tag!(LexicalElement::Comma) >>
                exp: call!(Exp::parse) >>
                (exp)
            )
        ) >>
        ({
            let mut rv = Vec::new();
            let mut rest = rest;
            rv.push(exp);
            rv.append(&mut rest);
            ExpList(rv)
        })
    ));

impl_parse!(Args, alt!(
        do_parse!(
            le_tag!(LexicalElement::OpenParen) >>
            el: call!(ExpList::parse) >>
            le_tag!(LexicalElement::CloseParen) >>
            (Args::ExpList(Box::new(el)))
        ) |
        do_parse!(
            le_tag!(LexicalElement::OpenParen) >>
            le_tag!(LexicalElement::CloseParen) >>
            (Args::Empty)
        ) |
        do_parse!(
            tc: call!(TableConstructor::parse) >>
            (Args::TableConstructor(Box::new(tc)))
        ) |
        le_tag!(LexicalElement::StringLiteral(a) => (Args::StringLiteral(escape_string_literal(a))))
    ));

impl_parse!(ParList, alt!(
        le_tag!(LexicalElement::Elipsis => ParList(Vec::new(), true)) |
        do_parse!(
            names:   call!(NameList::parse) >>
            elipsis: opt!(
                do_parse!(
                    le_tag!(LexicalElement::Comma)   >>
                    le_tag!(LexicalElement::Elipsis) >>
                    ()
                )
            ) >>
            ({
                let NameList(names) = names;
                ParList(names, Some(()) == elipsis)
            })
        )
    ));

impl_parse!(FuncName, do_parse!(
        name: le_tag!(LexicalElement::Identifier(a) => a.to_string()) >>
        rest: many0!(
            do_parse!(
                le_tag!(LexicalElement::Dot) >>
                name: le_tag!(LexicalElement::Identifier(a) => a.to_string()) >>
                (name)
            )
        ) >>
        last_name: opt!(
            do_parse!(
                le_tag!(LexicalElement::Colon) >>
                name: le_tag!(LexicalElement::Identifier(a) => a) >>
                (name.to_string())
            )
        ) >>
        ({
            let mut rv = Vec::new();
            let mut rest = rest;
            rv.push(name);
            rv.append(&mut rest);
            FuncName(rv, last_name)
        })
    ));

impl_parse!(FuncBody, do_parse!(
        le_tag!(LexicalElement::OpenParen)  >>
        params: opt!(call!(ParList::parse)) >>
        le_tag!(LexicalElement::CloseParen) >>
        chunk: call!(Chunk::parse) >>
        le_tag!(LexicalElement::Keyword("end")) >>
        ({
            let params = if let Some(v) = params {
                v
            } else {
                ParList(Vec::new(), false)
            };
            FuncBody(Box::new(params), Box::new(chunk))
        })
    ));

impl_parse!(Function, do_parse!(
        le_tag!(LexicalElement::Keyword("function")) >>
        fb: call!(FuncBody::parse) >>
        (Function(fb))
    ));

impl_parse!(NameAndArgs, do_parse!(
        name: opt!(do_parse!(
            le_tag!(LexicalElement::Colon) >>
            name: le_tag!(LexicalElement::Identifier(s) => s) >>
            (name.to_string())
        )) >>
        args: call!(Args::parse) >>
        (NameAndArgs(name, args))
    ));

impl_parse!(VarSuffix, alt!(
        do_parse!(
            naa: many0!(call!(NameAndArgs::parse)) >>
            le_tag!(LexicalElement::OpenSquare) >>
            exp: call!(Exp::parse) >>
            le_tag!(LexicalElement::CloseSquare) >>
            (VarSuffix::Index(naa, exp))
        ) |
        do_parse!(
            naa: many0!(call!(NameAndArgs::parse)) >>
            le_tag!(LexicalElement::Dot) >>
            name: le_tag!(LexicalElement::Identifier(s) => s.to_string()) >>
            (VarSuffix::Member(naa, name))
        )
    ));

impl_parse!(Var, alt!(
        do_parse!(
            name: le_tag!(LexicalElement::Identifier(s) => s.to_string()) >>
            vss: many0!(call!(VarSuffix::parse)) >>
            (Var::Name(name, vss))
        ) |
        do_parse!(
            le_tag!(LexicalElement::OpenParen) >>
            exp: call!(Exp::parse) >>
            le_tag!(LexicalElement::CloseParen) >>
            vss: many1!(call!(VarSuffix::parse)) >>
            (Var::Exp(exp, vss))
        )
    ));

impl_parse!(VarList, do_parse!(
        var: call!(Var::parse) >>
        rest: many0!(
            do_parse!(
                le_tag!(LexicalElement::Comma) >>
                name: call!(Var::parse) >>
                (name)
            )
        ) >>
        ({
            let mut rv = Vec::new();
            let mut rest = rest;
            rv.push(var);
            rv.append(&mut rest);
            VarList(rv)
        })
    ));

impl_parse!(Exp, alt!(
        do_parse!(uo: call!(UnOp::parse) >> 
                  ex: call!(Exp::parse)  >> 
                  (Exp::UnaryOp(uo, Box::new(ex))))   |
        do_parse!(e1: call!(SimpleExp::parse) >> 
                  bo: call!(BinOp::parse)>> 
                  e2: call!(Exp::parse)  >> 
                  (Exp::BinaryOp(Box::new(Exp::SimpleExp(Box::new(e1))), bo, Box::new(e2)))) |
        do_parse!(se: call!(SimpleExp::parse) >> 
                  (Exp::SimpleExp(Box::new(se))))     
    ));

impl_parse!(VarOrExp, alt!(
        do_parse!(
            var: call!(Var::parse) >>
            (VarOrExp::Var(var))
        ) |
        do_parse!(
            le_tag!(LexicalElement::OpenParen) >>
            exp: call!(Exp::parse) >>
            le_tag!(LexicalElement::CloseParen) >>
            (VarOrExp::Exp(exp))
        )
    ));

impl_parse!(PrefixExp, do_parse!(
        voe: call!(VarOrExp::parse) >>
        naa: many0!(call!(NameAndArgs::parse)) >>
        (PrefixExp(Box::new(voe), naa))
    ));

impl_parse!(FunctionCall, do_parse!(
        voe: call!(VarOrExp::parse) >>
        naa: many1!(call!(NameAndArgs::parse)) >>
        (FunctionCall(Box::new(voe), naa))
    ));

impl_parse!(Stat, alt!(
        do_parse!(
            vl: call!(VarList::parse) >>
            le_tag!(LexicalElement::Assign) >>
            el: call!(ExpList::parse) >>
            (Stat::Assign(Box::new(vl), Box::new(el)))
        ) |
        do_parse!(
            fc: call!(FunctionCall::parse) >>
            (Stat::FunctionCall(Box::new(fc)))
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("do")) >>
            chunk: call!(Chunk::parse) >>
            le_tag!(LexicalElement::Keyword("end")) >>
            (Stat::DoBlock(Box::new(chunk)))
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("while")) >>
            exp: call!(Exp::parse) >>
            le_tag!(LexicalElement::Keyword("do")) >>
            chunk: call!(Chunk::parse) >>
            le_tag!(LexicalElement::Keyword("end")) >>
            (Stat::WhileBlock(Box::new(exp), Box::new(chunk)))
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("repeat")) >>
            chunk: call!(Chunk::parse) >>
            le_tag!(LexicalElement::Keyword("until")) >>
            exp: call!(Exp::parse) >>
            (Stat::RepeatBlock(Box::new(exp), Box::new(chunk)))
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("if")) >>
            exp1: call!(Exp::parse) >>
            le_tag!(LexicalElement::Keyword("then")) >>
            chunk1: call!(Chunk::parse) >>
            rest: many0!(do_parse!(
                le_tag!(LexicalElement::Keyword("elseif")) >>
                exp: call!(Exp::parse) >>
                le_tag!(LexicalElement::Keyword("then")) >>
                chunk: call!(Chunk::parse) >>
                ((exp, chunk))
            )) >>
            else_chunk: opt!(do_parse!(
                le_tag!(LexicalElement::Keyword("else")) >>
                ch: call!(Chunk::parse) >>
                (ch)
            )) >>
            le_tag!(LexicalElement::Keyword("end")) >>
            ({
                let mut v = vec![(exp1, chunk1)];
                let mut rest = rest;
                v.append(&mut rest);
                Stat::IfElseBlock(v, else_chunk.map(|x| Box::new(x)))
            })
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("for")) >>
            name: le_tag!(LexicalElement::Identifier(s) => s.to_string()) >>
            le_tag!(LexicalElement::Assign) >>
            e1: call!(Exp::parse) >>
            le_tag!(LexicalElement::Comma) >>
            e2: call!(Exp::parse) >>
            e3: opt!(do_parse!(
                le_tag!(LexicalElement::Comma) >>
                e: call!(Exp::parse) >>
                (e)
            )) >>
            le_tag!(LexicalElement::Keyword("do")) >>
            ch: call!(Chunk::parse) >>
            le_tag!(LexicalElement::Keyword("end")) >>
            ({
                let mut ev = vec![e1, e2];
                if let Some(v) = e3 {
                    ev.push(v);
                }
                Stat::ForRangeBlock(name, ev, Box::new(ch))
            })
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("for")) >>
            nl: call!(NameList::parse) >>
            le_tag!(LexicalElement::Keyword("in")) >>
            el: call!(ExpList::parse) >>
            le_tag!(LexicalElement::Keyword("do")) >>
            ch: call!(Chunk::parse) >>
            le_tag!(LexicalElement::Keyword("end")) >>
            (Stat::ForInBlock(Box::new(nl), Box::new(el), Box::new(ch)))
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("function")) >>
            name: call!(FuncName::parse) >>
            body: call!(FuncBody::parse) >>
            (Stat::FunctionDec(name, Box::new(body)))
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("local")) >>
            le_tag!(LexicalElement::Keyword("function")) >>
            name: le_tag!(LexicalElement::Identifier(s) => s.to_string()) >>
            body: call!(FuncBody::parse) >>
            (Stat::LocalFunctionDec(name, Box::new(body)))
        ) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("local")) >>
            nl: call!(NameList::parse) >>
            el: opt!(do_parse!(
                le_tag!(LexicalElement::Assign) >>
                el: call!(ExpList::parse) >>
                (el)
            )) >> 
            (Stat::LocalAssign(Box::new(nl), (if let Some(el) = el {
                el
            } else {
                ExpList(Vec::new())
            })))
        )
    ));

// all ok beyond this point

impl_parse!(LastStat, alt!(
        le_tag!(LexicalElement::Keyword("break") => LastStat::Break) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("return")) >>
            es: opt!(call!(ExpList::parse)) >>
            (
                match es {
                    Some(es) => LastStat::Return(Box::new(es)),
                    None => LastStat::Return(Box::new(ExpList(Vec::new()))),
                }
            )
        )
    ));

impl_parse!(Chunk, do_parse!(
        ss: many0!(
            do_parse!(
                s: call!(Stat::parse) >>
                opt!(le_tag!(LexicalElement::Semicolon)) >>
                (s)
            )
        ) >>
        ls: opt!(
            do_parse!(
                s: call!(LastStat::parse) >>
                opt!(le_tag!(LexicalElement::Semicolon)) >>
                (s)
            )
        ) >>
        (Chunk(ss, ls))
    ));

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
            },
            e => Exp::UnaryOp(uo, Box::new(e)),
        }
    }
    fn descend_binary_op_on_right(&mut self, e: Exp, bo: BinOp, re: Exp) -> Exp {
        let mut recurse_right = |e, bo, re| {
            match e {
                Exp::BinaryOp(ile, ibo, ire) => Exp::BinaryOp(
                    ile, 
                    ibo, 
                    Box::new(self.descend_binary_op_on_right(*ire, bo, re))
                ),
                Exp::UnaryOp(iuo, ire) => Exp::UnaryOp(
                    iuo, 
                    Box::new(self.descend_binary_op_on_right(*ire, bo, re))
                ),
                _ => unreachable!(),
            }
        };
        match bo.precedence().cmp(&e.precedence()) {
            Greater => recurse_right(e, bo, re),
            Equal => if bo.associativity() == Right {
                recurse_right(e, bo, re)
            } else {
                Exp::BinaryOp(Box::new(e), bo, Box::new(re))
            },
            Less => Exp::BinaryOp(Box::new(e), bo, Box::new(re)),
        }
    }
    fn descend_binary_op_on_left(&mut self, e: Exp, bo: BinOp, le: Exp) -> Exp {
        let mut recurse_left = |e, bo, le| {
            match e {
                Exp::BinaryOp(ile, ibo, ire) => Exp::BinaryOp(
                    Box::new(self.descend_binary_op_on_left(*ile, bo, le)),
                    ibo, 
                    ire
                ), 
                e@Exp::UnaryOp(_, _) => Exp::BinaryOp(Box::new(le), bo, Box::new(e)),
                _ => unreachable!(),
            }
        };
        match bo.precedence().cmp(&e.precedence()) {
            Greater => recurse_left(e, bo, le),
            Equal => if bo.associativity() == Left {
                recurse_left(e, bo, le)
            } else {
                Exp::BinaryOp(Box::new(le), bo, Box::new(e))
            },
            Less => Exp::BinaryOp(Box::new(le), bo, Box::new(e)),
        }
    }
    // This is gonna be awful from a performance perspective, but I had no idea 
    // how else to do this
    fn  destructure_left_subtree(&mut self, e: Exp, precedence: u8) -> (Exp, Box<FnMut(Exp) -> Exp>) {
        match e {
            e@Exp::SimpleExp(_) | e@Exp::UnaryOp(_, _) => {
                (e, Box::new(move |e: Exp| e))
            }
            Exp::BinaryOp(le, op, re) => {
                macro_rules! descend_left {
                    () => {{
                        let mut re = Some(re);
                        let (ie, mut ifn) = self.destructure_left_subtree(*le, precedence);
                        (ie, Box::new(move |pe: Exp| {
                            Exp::BinaryOp(
                                Box::new(ifn(pe)),
                                op,
                                re.take().unwrap()
                            )
                        }))
                    }};
                }
                match op.precedence().cmp(&precedence) {
                    Less => descend_left!(),
                    Equal if op.associativity() == Left => descend_left!(),
                    _ => {
                        (
                            Exp::BinaryOp(le, op, re), 
                            Box::new(move |e: Exp| e)
                        )
                    }
                }
            }
        }
    }
    fn destructure_right_subtree(&mut self, e: Exp, precedence: u8) -> (Exp, Box<FnMut(Exp) -> Exp>) {
        match e {
            e@Exp::SimpleExp(_) => {
                (e, Box::new(move |e: Exp| e))
            }
            Exp::UnaryOp(uo, ie) => {
                match UnOp::precedence().cmp(&precedence) {
                    Greater => {
                        let (ie, mut ifn) = self.destructure_right_subtree(*ie, precedence);
                        (ie, Box::new(move |pe: Exp| {
                            Exp::UnaryOp(
                                uo,
                                Box::new(ifn(pe)),
                            )
                        }))
                    },
                    Equal => unreachable!(),
                    Less => (Exp::UnaryOp(uo, ie), Box::new(move |e: Exp| e)),

                }
            }
            Exp::BinaryOp(le, op, re) => {
                macro_rules! descend_right {
                    () => {{
                        let mut le = Some(le);
                        let (ie, mut ifn) = self.destructure_right_subtree(*re, precedence);
                        (ie, Box::new(move |pe: Exp| {
                            Exp::BinaryOp(
                                le.take().unwrap(),
                                op,
                                Box::new(ifn(pe))
                            )
                        }))
                    }};
                }
                match op.precedence().cmp(&precedence) {
                    Less => descend_right!(),
                    Equal if op.associativity() == Right => descend_right!(),
                    _ => {
                        (
                            Exp::BinaryOp(le, op, re), 
                            Box::new(move |e: Exp| e)
                        )
                    }
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
                Exp::UnaryOp(op, mut ce) => {
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


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ParseError {
    Error
}

#[allow(dead_code)]
pub fn parse_lua_source(input: &[u8]) -> Result<Chunk, ParseError> {
    //let tokens = lex::tokenify_string(input).map_err(|_|ParseError::Error)?;
    let tokens = lex::tokenify_string(input).unwrap();
    parse_chunk(&tokens)
}

#[allow(dead_code)]
pub fn parse_chunk(input: &[LexicalElement]) -> Result<Chunk, ParseError> {
    let (_, mut parsed) = match Chunk::parse(input) {
        IResult::Done(np, mut p) => (np, p),
        _ => return Err(ParseError::Error),
    };
    let mut ebv = ExpBalanceVisitor::default();
    ebv.visit_chunk(&mut parsed);
    Ok(parsed)
}

#[allow(dead_code)]
fn parse_exp(input: &[LexicalElement]) -> Result<Exp, ParseError> {
    let (_, mut parsed) = match Exp::parse(input) {
        IResult::Done(np, mut p) => (np, p),
        _ => return Err(ParseError::Error),
    };
    let mut ebv = ExpBalanceVisitor::default();
    ebv.visit_exp(&mut parsed);
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use types::*;
    use super::*;

    macro_rules! tree {
        (+, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Plus,
            Box::new($b)
        ));
        (-, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Minus,
            Box::new($b)
        ));
        (*, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Mult,
            Box::new($b)
        ));
        (/, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Div,
            Box::new($b)
        ));
        (^, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Pow,
            Box::new($b)
        ));
        (%, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Mod,
            Box::new($b)
        ));
        (<, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::LessThan,
            Box::new($b)
        ));
        (>, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::GreaterThan,
            Box::new($b)
        ));
        (=, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Equals,
            Box::new($b)
        ));
        (.., $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Concat,
            Box::new($b)
        ));
        (<=, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::LessEqual,
            Box::new($b)
        ));
        (>=, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::GreaterEqual,
            Box::new($b)
        ));
        (!=, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::NotEquals,
            Box::new($b)
        ));
        (&&, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::And,
            Box::new($b)
        ));
        (||, $a:expr, $b:expr) => (Exp::BinaryOp(
            Box::new($a),
            BinOp::Or,
            Box::new($b)
        ));
        (-, $e:expr) => (Exp::UnaryOp(
            UnOp::Neg,
            Box::new($e)
        ));
        (!, $e:expr) => (Exp::UnaryOp(
            UnOp::Not,
            Box::new($e)
        ));
        (#, $e:expr) => (Exp::UnaryOp(
            UnOp::Len,
            Box::new($e)
        ));
        ($e:expr) => (Exp::SimpleExp(Box::new(SimpleExp::Number($e))));
        () => (Exp::SimpleExp(Box::new(SimpleExp::Nil)));
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
        //{
        //    let mut et = tree!(+, tree!(||, tree!(), tree!()), tree!());
        //    println!("Before:\n{:#?}", et);
        //    fix_exp_tree(&mut et);
        //    println!("After:\n{:#?}", et);
        //}
        
        //"nil"
        assert_transform!(
            tree!(
            ),
            tree!(
            )
        );

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
                tree!(
                    ..,
                    tree!(3.),
                    tree!(4.)
                )
            ),
            tree!(
                ..,
                tree!(
                    +,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(
                    ..,
                    tree!(3.),
                    tree!(4.)
                )
            )
        );

        //"1.0 .. 2.0 .. 3.0"
        assert_transform!(
            tree!(
                ..,
                tree!(
                    ..,
                    tree!(1.),
                    tree!(2.)
                ),
                tree!(3.)
            ),
            tree!(
                ..,
                tree!(1.),
                tree!(
                    ..,
                    tree!(2.),
                    tree!(3.)
                )
            )
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
                    tree!(2.),
                    tree!(
                        ..,
                        tree!(3.),
                        tree!(4.)
                    )
                )
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
    fn test_parser() {
        {
            let input = b"4.0 + - nil ^ nil";
            let input = lex::tokenify_string(&input[..]).unwrap();
            let (_, output) = Exp::parse(&input).unwrap();
            let expected =
                tree!(
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
            let (_, exp) = Exp::parse(
                &lex::tokenify_string(b"4 + 4 ^ 4 + 4").unwrap()).unwrap();
            assert_eq!(exp, tree!(+, tree!(4.0), tree!(^, tree!(4.0), 
                tree!(+, tree!(4.0), tree!(4.0)))));
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
}
