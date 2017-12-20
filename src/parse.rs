use nom::*;
use std::str;
use types::*;

// --------------- Some Helper Macros ------------------

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
                SimpleExp::Number(parse_number(a)))
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
            el: call!(ExpList::parse) >>
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

// TODO: Finish this
impl_parse!(Exp, alt!(
        do_parse!(uo: call!(UnOp::parse) >> 
                  ex: call!(Exp::parse)  >> 
                  (Exp::UnaryOp(uo, Box::new(ex))))   |
        do_parse!(e1: call!(SimpleExp::parse) >> 
                  bo: call!(BinOp::parse)>> 
                  e2: call!(Exp::parse)  >> 
                  (Exp::BinaryOp(Box::new(Exp::SimpleExp(Box::new(e1))), bo, Box::new(e2)))) |
        do_parse!(se: call!(SimpleExp::parse) >> 
                  (Exp::SimpleExp(Box::new(se))))     |
        do_parse!(tc: call!(TableConstructor::parse) >> 
                  (Exp::TableConstructor(Box::new(tc))))               
    ));

// TODO: Resolve the precedence heirarchy in the Exp tree at some point

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
        //println!("{:?}", Exp::parse(&input));
    }
}
