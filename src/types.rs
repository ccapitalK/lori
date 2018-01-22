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
    // greater precedence means evaluated first
    fn precedence(&self) -> u8 {
        match *self {
            BinOp::Or => 0,
            BinOp::And => 1,
            BinOp::LessThan | BinOp::LessEqual | 
                BinOp::Equals | BinOp::NotEquals | 
                BinOp::GreaterThan | BinOp::GreaterEqual => 2,
            BinOp::Concat => 3,
            BinOp::Plus | BinOp::Minus => 4,
            BinOp::Mult | BinOp::Div | BinOp::Mod => 5,
            BinOp::Pow => 7,
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
    TableConstructor(Box<TableConstructor>),
    PrefixExp(Box<PrefixExp>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExp(pub Box<VarOrExp>, pub Vec<NameAndArgs>);

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    SimpleExp(Box<SimpleExp>),
    UnaryOp(UnOp, Box<Exp>),
    BinaryOp(Box<Exp>, BinOp, Box<Exp>),
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
    Empty,
    ExpList(Box<ExpList>),
    TableConstructor(Box<TableConstructor>),
    StringLiteral(String)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function(pub FuncBody);

#[derive(Debug, Clone, PartialEq)]
pub struct FuncBody(pub Box<ParList>, pub Box<Chunk>);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall(pub Box<VarOrExp>, pub Vec<NameAndArgs>);

#[derive(Debug, Clone, PartialEq)]
pub enum Var{
    Name(String, Vec<VarSuffix>),
    Exp(Exp, Vec<VarSuffix>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarList(pub Vec<Var>);

#[derive(Debug, Clone, PartialEq)]
pub enum Stat{
    Assign(Box<VarList>, Box<ExpList>),
    FunctionCall(Box<FunctionCall>),
    DoBlock(Box<Chunk>),
    WhileBlock(Box<Exp>, Box<Chunk>),
    RepeatBlock(Box<Exp>, Box<Chunk>),
    IfElseBlock(Vec<(Exp, Chunk)>, Option<Box<Chunk>>),
    ForRangeBlock(String, Vec<Exp>, Box<Chunk>),
    ForInBlock(Box<NameList>, Box<ExpList>, Box<Chunk>),
    FunctionDec(FuncName, Box<FuncBody>),
    LocalFunctionDec(String, Box<FuncBody>),
    LocalAssign(Box<NameList>, Option<ExpList>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LastStat{
    Return(Box<ExpList>),
    Break
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(pub Vec<Stat>, pub Option<LastStat>);

#[derive(Debug, Clone, PartialEq)]
pub struct NameAndArgs(pub Option<String>, pub Args);

#[derive(Debug, Clone, PartialEq)]
pub enum VarSuffix{
    Index(Vec<NameAndArgs>, Exp),
    Member(Vec<NameAndArgs>, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarOrExp{
    Var(Var),
    Exp(Exp),
}

pub trait ASTVisitor<T> {
    fn visit_name(&mut self, _: &str) -> Option<T> {
        None
    }
    fn visit_unary_op(&mut self, _: &UnOp) -> Option<T> {
        None
    }
    fn visit_binary_op(&mut self, _: &BinOp) -> Option<T> {
        None
    }
    fn visit_field_sep(&mut self, _: &FieldSep) -> Option<T> {
        None
    }
    fn visit_simple_exp(&mut self, se: &SimpleExp) -> Option<T> {
        match se {
            &SimpleExp::TableConstructor(ref b) => self.visit_table_constructor(b.as_ref()),
            &SimpleExp::PrefixExp(ref b) => self.visit_prefix_exp(b.as_ref()),
            _ => None,
        }
    }
    fn visit_prefix_exp(&mut self, pe: &PrefixExp) -> Option<T> {
        self.visit_var_or_exp(pe.0.as_ref());
        for naa in pe.1.iter() {
            self.visit_name_and_args(naa);
        }
        None
    }
    fn visit_exp(&mut self, pe: &Exp) -> Option<T> {
        match pe {
            &Exp::SimpleExp(ref b) => self.visit_simple_exp(b.as_ref()),
            &Exp::UnaryOp(ref u, ref b) => {
                self.visit_unary_op(u); 
                self.visit_exp(b.as_ref()); 
                None
            },
            &Exp::BinaryOp(ref b1, ref bo, ref b2) => {
                self.visit_exp(b1.as_ref()); 
                self.visit_binary_op(bo); 
                self.visit_exp(b2.as_ref()); 
                None
            }, 
        }
    }
    fn visit_field(&mut self, f: &Field) -> Option<T> {
        match f {
            &Field::Exp(ref b) => self.visit_exp(b.as_ref()),
            &Field::NamedExp(ref s, ref b) => {
                self.visit_name(s.as_str());
                self.visit_exp(b.as_ref());
                None
            },
            &Field::IndexExp(ref b1, ref b2) =>{
                self.visit_exp(b1.as_ref());
                self.visit_exp(b2.as_ref());
                None
            },
        }
    }
    fn visit_table_constructor(&mut self, tc: &TableConstructor) -> Option<T> {
        for field in tc.0.iter() {
            self.visit_field(field);
        }
        None
    }
    fn visit_name_list(&mut self, nl: &NameList) -> Option<T> {
        for name in nl.0.iter() {
            self.visit_name(name);
        }
        None
    }
    fn visit_par_list(&mut self, pl: &ParList) -> Option<T> {
        for parameter in pl.0.iter() {
            self.visit_name(parameter);
        }
        None
    }
    fn visit_func_name(&mut self, f: &FuncName) -> Option<T> {
        for name in f.0.iter() {
            self.visit_name(name);
        }
        if let &Some(ref v) = &f.1 {
            self.visit_name(v.as_str());
        }
        None
    }
    fn visit_exp_list(&mut self, el: &ExpList) -> Option<T> {
        for exp in el.0.iter() {
            self.visit_exp(exp);
        }
        None
    }
    fn visit_args(&mut self, a: &Args) -> Option<T> {
        match a {
            &Args::ExpList(ref b) => self.visit_exp_list(b.as_ref()),
            &Args::TableConstructor(ref b) => self.visit_table_constructor(b.as_ref()),
            _ => None,
        }
    }
    fn visit_function(&mut self, f: &Function) -> Option<T> {
        self.visit_func_body(&f.0)
    }
    fn visit_func_body(&mut self, fb: &FuncBody) -> Option<T> {
        self.visit_par_list(fb.0.as_ref());
        self.visit_chunk(fb.1.as_ref());
        None
    }
    fn visit_function_call(&mut self, fc: &FunctionCall) -> Option<T> {
        self.visit_var_or_exp(fc.0.as_ref());
        for name_and_args in fc.1.iter() {
            self.visit_name_and_args(name_and_args);
        }
        None
    }
    fn visit_var(&mut self, v: &Var) -> Option<T> {
        match v {
            &Var::Name(ref n, ref vss) => {
                self.visit_name(n.as_str());
                for vs in vss.iter(){
                    self.visit_var_suffix(vs);
                }
            },
            &Var::Exp(ref e, ref vss) => {
                self.visit_exp(e);
                for vs in vss.iter(){
                    self.visit_var_suffix(vs);
                }
            },
        };
        None
    }
    fn visit_var_list(&mut self, vl: &VarList) -> Option<T> {
        for v in vl.0.iter() {
            self.visit_var(v);
        }
        None
    }
    fn visit_stat(&mut self, s: &Stat) -> Option<T> {
        match s {
            &Stat::Assign(ref vl, ref el) => {
                for v in vl.0.iter() {
                    self.visit_var(v);
                }
                for e in el.0.iter() {
                    self.visit_exp(e);
                }
                None
            },
            &Stat::FunctionCall(ref fc) => self.visit_function_call(fc.as_ref()),
            &Stat::DoBlock(ref ch) => self.visit_chunk(ch.as_ref()),
            &Stat::WhileBlock(ref e, ref ch) => {
                self.visit_exp(e);
                self.visit_chunk(ch);
                None
            },
            &Stat::RepeatBlock(ref e, ref ch) => {
                self.visit_exp(e);
                self.visit_chunk(ch);
                None
            },
            &Stat::IfElseBlock(ref v, ref och) => {
                for &(ref e, ref ch) in v.iter() {
                    self.visit_exp(e);
                    self.visit_chunk(ch);
                }
                if let &Some(ref och) = och {
                    self.visit_chunk(och);
                }
                None
            },
            &Stat::ForRangeBlock(ref n, ref el, ref ch) => {
                self.visit_name(n.as_str());
                for e in el.iter() {
                    self.visit_exp(e);
                }
                self.visit_chunk(ch.as_ref());
                None
            },
            &Stat::ForInBlock(ref nl, ref el, ref ch) => {
                self.visit_name_list(nl.as_ref());
                self.visit_exp_list(el.as_ref());
                self.visit_chunk(ch.as_ref());
                None
            },
            &Stat::FunctionDec(ref n, ref fb) => {
                self.visit_func_name(n);
                self.visit_func_body(fb.as_ref());
                None
            },
            &Stat::LocalFunctionDec(ref n, ref fb) => {
                self.visit_name(n.as_str());
                self.visit_func_body(fb.as_ref());
                None
            },
            &Stat::LocalAssign(ref nl, ref el) => {
                for n in nl.0.iter() {
                    self.visit_name(n);
                }
                if let &Some(ref el) = el {
                    for e in el.0.iter() {
                        self.visit_exp(e);
                    }
                }
                None
            },
        }
    }
    fn visit_last_stat(&mut self, ls: &LastStat) -> Option<T> {
        match ls {
            &LastStat::Return(ref b) => self.visit_exp_list(b.as_ref()),
            &LastStat::Break => None,
        }
    }
    fn visit_chunk(&mut self, ch: &Chunk) -> Option<T> {
        for s in ch.0.iter() {
            self.visit_stat(s);
        }
        if let &Some(ref ls) = &ch.1 {
            self.visit_last_stat(ls);
        }
        None
    }
    fn visit_name_and_args(&mut self, naa: &NameAndArgs) -> Option<T> {
        if let &Some(ref s) = &naa.0 {
            self.visit_name(s.as_str());
        }
        self.visit_args(&naa.1);
        None
    }
    fn visit_var_suffix(&mut self, vs: &VarSuffix) -> Option<T> {
        match vs {
            &VarSuffix::Index(ref naas, ref e) => {
                for naa in naas.iter() {
                    self.visit_name_and_args(naa);
                }
                self.visit_exp(e);
            },
            &VarSuffix::Member(ref naas, ref s) => {
                for naa in naas.iter() {
                    self.visit_name_and_args(naa);
                }
                self.visit_name(s.as_str());
            },
        };
        None
    }
    fn visit_var_or_exp(&mut self, voe: &VarOrExp) -> Option<T> {
        match voe {
            &VarOrExp::Var(ref v) => self.visit_var(v),
            &VarOrExp::Exp(ref e) => self.visit_exp(e),
        }
    }
}
