use nom::*;
use std::str;
use types::*;

// ---------------- Actual parsers ---------------------

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
}

impl_parse!(UnOp, alt!(
        value!(UnOp::Neg, le_tag!(LexicalElement::Minus)) |
        value!(UnOp::Not, le_tag!(LexicalElement::Keyword("not"))) |
        value!(UnOp::Len, le_tag!(LexicalElement::Hash))
    ));

impl_parse!(BinOp, alt!(
        value!(BinOp::Plus        , le_tag!(LexicalElement::Plus          ))|         
        value!(BinOp::Minus       , le_tag!(LexicalElement::Minus         ))|          
        value!(BinOp::Mult        , le_tag!(LexicalElement::Mult          ))|         
        value!(BinOp::Div         , le_tag!(LexicalElement::Div           ))|        
        value!(BinOp::Pow         , le_tag!(LexicalElement::Caret         ))|        
        value!(BinOp::Mod         , le_tag!(LexicalElement::Mod           ))|        
        value!(BinOp::Concat      , le_tag!(LexicalElement::Concat        ))|           
        value!(BinOp::LessEqual   , le_tag!(LexicalElement::LessEqual     ))|              
        value!(BinOp::LessThan    , le_tag!(LexicalElement::LessThan      ))|             
        value!(BinOp::GreaterThan , le_tag!(LexicalElement::GreaterThan   ))|                
        value!(BinOp::GreaterEqual, le_tag!(LexicalElement::GreaterEqual  ))|                 
        value!(BinOp::Equals      , le_tag!(LexicalElement::Equals        ))|           
        value!(BinOp::NotEquals   , le_tag!(LexicalElement::NotEquals     ))|              
        value!(BinOp::And         , le_tag!(LexicalElement::Keyword("and")))|        
        value!(BinOp::Or          , le_tag!(LexicalElement::Keyword("or") ))        
    ));

fn escape_string_literal(a: LexicalElement) -> Option<SimpleExp> {
    if let LexicalElement::StringLiteral(a) = a {
        Some(SimpleExp::StringLiteral(a.to_string()))
    } else {
        None
    }
}

fn parse_number(a: LexicalElement) -> Option<SimpleExp> {
    if let LexicalElement::Number(a) = a {
        //TODO: Improve this
        Some(SimpleExp::Number(a.parse::<f64>().unwrap()))
    } else {
        None
    }
}

impl_parse!(SimpleExp, alt!(
        value!(SimpleExp::Nil, le_tag!(LexicalElement::Keyword("nil")))     |
        value!(SimpleExp::False, le_tag!(LexicalElement::Keyword("false"))) |
        value!(SimpleExp::True, le_tag!(LexicalElement::Keyword("true")))   |
        value!(SimpleExp::Elipsis, le_tag!(LexicalElement::Elipsis))        |
        map_opt!(le_tag!(LexicalElement::StringLiteral(_)), 
                 escape_string_literal)                                     |
        map_opt!(le_tag!(LexicalElement::Number(_)), parse_number)           
    ));

impl_parse!(Exp, alt!(
        do_parse!(uo: call!(UnOp::parse) >> 
                  ex: call!(Exp::parse)  >> 
                  (Exp::UnaryOp(uo, Box::new(ex))))   |
        do_parse!(e1: call!(SimpleExp::parse) >> 
                  bo: call!(BinOp::parse)>> 
                  e2: call!(Exp::parse)  >> 
                  (Exp::BinaryOp(Box::new(Exp::SimpleExp(e1)), bo, Box::new(e2)))) |
        do_parse!(se: call!(SimpleExp::parse) >> 
                  (Exp::SimpleExp(se)))               
    ));

// TODO: Resolve the precedence heirarchy in the Exp tree at some point

#[test]
fn test_parser() {
    use lex::tokenify_string;
    //let input = b"\"Hello world\"";
    {
        let input = b"4.0 + - nil ^ nil";
        let input = tokenify_string(&input[..]).unwrap();
        let (_, output) = Exp::parse(&input).unwrap();
        let expected =
             Exp::BinaryOp(
                 Box::new(Exp::SimpleExp(SimpleExp::Number(4.0))),
                 BinOp::Plus,
                 Box::new(Exp::UnaryOp(UnOp::Neg, Box::new(Exp::BinaryOp(
                     Box::new(Exp::SimpleExp(SimpleExp::Nil)),
                     BinOp::Pow,
                     Box::new(Exp::SimpleExp(SimpleExp::Nil))
                 ))))
             ); 
        assert_eq!(expected, output);
        //println!("{:?}", Exp::parse(&input));
    }
}
