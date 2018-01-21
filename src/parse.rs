use nom::*;
use std::str;
use types::*;

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

// TODO: Resolve the precedence heirarchy in the Exp tree at some point

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
            (Stat::LocalAssign(Box::new(nl), el))
        )
    ));

// all ok beyond this point

impl_parse!(LastStat, alt!(
        le_tag!(LexicalElement::Keyword("break") => LastStat::Break) |
        do_parse!(
            le_tag!(LexicalElement::Keyword("return")) >>
            es: call!(ExpList::parse) >>
            (LastStat::Return(Box::new(es)))
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

#[test]
fn test_parser() {
    use lex::tokenify_string;
    //let input = b"\"Hello world\"";
    //use std::mem::size_of;
    //println!("{}", size_of::<Field>());
    {
        let input = b"4.0 + - nil ^ nil";
        let input = tokenify_string(&input[..]).unwrap();
        let (_, output) = Exp::parse(&input).unwrap();
        let expected =
             Exp::BinaryOp(
                 Box::new(Exp::SimpleExp(Box::new(SimpleExp::Number(4.0)))),
                 BinOp::Plus,
                 Box::new(Exp::UnaryOp(UnOp::Neg, Box::new(Exp::BinaryOp(
                     Box::new(Exp::SimpleExp(Box::new(SimpleExp::Nil))),
                     BinOp::Pow,
                     Box::new(Exp::SimpleExp(Box::new(SimpleExp::Nil)))
                 ))))
             ); 
        assert_eq!(expected, output);
    }
}
