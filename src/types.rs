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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LeftOrRight {
    Left,
    Right
}

impl LeftOrRight {
    pub fn invert(self) -> Self {
        match self {
            LeftOrRight::Left => LeftOrRight::Right,
            LeftOrRight::Right => LeftOrRight::Left,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
    Len
}

impl UnOp {
    pub fn precedence() -> u8 {
        7
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    // greater precedence means evaluated first (ie. closer to being a leaf
    // of the exp tree)
    pub fn precedence(&self) -> u8 {
        match *self {
            BinOp::Or => 1,
            BinOp::And => 2,
            BinOp::LessThan | BinOp::LessEqual | 
                BinOp::Equals | BinOp::NotEquals | 
                BinOp::GreaterThan | BinOp::GreaterEqual => 3,
            BinOp::Concat => 4,
            BinOp::Plus | BinOp::Minus => 5,
            BinOp::Mult | BinOp::Div | BinOp::Mod => 6,
            BinOp::Pow => 8,
        }
    }
    pub fn associativity(&self) -> LeftOrRight {
        match *self {
            BinOp::Concat | BinOp::Pow => LeftOrRight::Right,
            _ => LeftOrRight::Left,
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

impl Exp {
    pub fn precedence(&self) -> u8 {
        match self {
            &Exp::SimpleExp(_) => 9,
            &Exp::UnaryOp(_, _) => UnOp::precedence(),
            &Exp::BinaryOp(_, ref o, _) => o.precedence(),
        }
    }
    pub fn associativity(&self) -> Option<LeftOrRight> {
        match self {
            &Exp::SimpleExp(_) => None,
            &Exp::UnaryOp(_, _) => None,
            &Exp::BinaryOp(_, ref o, _) => Some(o.associativity()),
        }
    }
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
    fn visit_name(&mut self, _: &mut String) -> Option<T> {
        None
    }
    fn visit_unary_op(&mut self, _: &mut UnOp) -> Option<T> {
        None
    }
    fn visit_binary_op(&mut self, _: &mut BinOp) -> Option<T> {
        None
    }
    fn visit_field_sep(&mut self, _: &mut FieldSep) -> Option<T> {
        None
    }
    fn visit_simple_exp(&mut self, se: &mut SimpleExp) -> Option<T> {
        match se {
            &mut SimpleExp::TableConstructor(ref mut b) => self.visit_table_constructor(b.as_mut()),
            &mut SimpleExp::PrefixExp(ref mut b) => self.visit_prefix_exp(b.as_mut()),
            _ => None,
        }
    }
    fn visit_prefix_exp(&mut self, pe: &mut PrefixExp) -> Option<T> {
        self.visit_var_or_exp(pe.0.as_mut());
        for naa in pe.1.iter_mut() {
            self.visit_name_and_args(naa);
        }
        None
    }
    fn visit_exp(&mut self, pe: &mut Exp) -> Option<T> {
        match pe {
            &mut Exp::SimpleExp(ref mut b) => self.visit_simple_exp(b.as_mut()),
            &mut Exp::UnaryOp(ref mut u, ref mut b) => {
                self.visit_unary_op(u); 
                self.visit_exp(b.as_mut()); 
                None
            },
            &mut Exp::BinaryOp(ref mut b1, ref mut bo, ref mut b2) => {
                self.visit_exp(b1.as_mut()); 
                self.visit_binary_op(bo); 
                self.visit_exp(b2.as_mut()); 
                None
            }, 
        }
    }
    fn visit_field(&mut self, f: &mut Field) -> Option<T> {
        match f {
            &mut Field::Exp(ref mut b) => self.visit_exp(b.as_mut()),
            &mut Field::NamedExp(ref mut s, ref mut b) => {
                self.visit_name(s);
                self.visit_exp(b.as_mut());
                None
            },
            &mut Field::IndexExp(ref mut b1, ref mut b2) =>{
                self.visit_exp(b1.as_mut());
                self.visit_exp(b2.as_mut());
                None
            },
        }
    }
    fn visit_table_constructor(&mut self, tc: &mut TableConstructor) -> Option<T> {
        for field in tc.0.iter_mut() {
            self.visit_field(field);
        }
        None
    }
    fn visit_name_list(&mut self, nl: &mut NameList) -> Option<T> {
        for name in nl.0.iter_mut() {
            self.visit_name(name);
        }
        None
    }
    fn visit_par_list(&mut self, pl: &mut ParList) -> Option<T> {
        for parameter in pl.0.iter_mut() {
            self.visit_name(parameter);
        }
        None
    }
    fn visit_func_name(&mut self, f: &mut FuncName) -> Option<T> {
        for name in f.0.iter_mut() {
            self.visit_name(name);
        }
        if let &mut Some(ref mut v) = &mut f.1 {
            self.visit_name(v);
        }
        None
    }
    fn visit_exp_list(&mut self, el: &mut ExpList) -> Option<T> {
        for exp in el.0.iter_mut() {
            self.visit_exp(exp);
        }
        None
    }
    fn visit_args(&mut self, a: &mut Args) -> Option<T> {
        match a {
            &mut Args::ExpList(ref mut b) => self.visit_exp_list(b.as_mut()),
            &mut Args::TableConstructor(ref mut b) => self.visit_table_constructor(b.as_mut()),
            _ => None,
        }
    }
    fn visit_function(&mut self, f: &mut Function) -> Option<T> {
        self.visit_func_body(&mut f.0)
    }
    fn visit_func_body(&mut self, fb: &mut FuncBody) -> Option<T> {
        self.visit_par_list(fb.0.as_mut());
        self.visit_chunk(fb.1.as_mut());
        None
    }
    fn visit_function_call(&mut self, fc: &mut FunctionCall) -> Option<T> {
        self.visit_var_or_exp(fc.0.as_mut());
        for name_and_args in fc.1.iter_mut() {
            self.visit_name_and_args(name_and_args);
        }
        None
    }
    fn visit_var(&mut self, v: &mut Var) -> Option<T> {
        match v {
            &mut Var::Name(ref mut n, ref mut vss) => {
                self.visit_name(n);
                for vs in vss.iter_mut(){
                    self.visit_var_suffix(vs);
                }
            },
            &mut Var::Exp(ref mut e, ref mut vss) => {
                self.visit_exp(e);
                for vs in vss.iter_mut(){
                    self.visit_var_suffix(vs);
                }
            },
        };
        None
    }
    fn visit_var_list(&mut self, vl: &mut VarList) -> Option<T> {
        for v in vl.0.iter_mut() {
            self.visit_var(v);
        }
        None
    }
    fn visit_stat(&mut self, s: &mut Stat) -> Option<T> {
        match s {
            &mut Stat::Assign(ref mut vl, ref mut el) => {
                for v in vl.0.iter_mut() {
                    self.visit_var(v);
                }
                for e in el.0.iter_mut() {
                    self.visit_exp(e);
                }
                None
            },
            &mut Stat::FunctionCall(ref mut fc) => self.visit_function_call(fc.as_mut()),
            &mut Stat::DoBlock(ref mut ch) => self.visit_chunk(ch.as_mut()),
            &mut Stat::WhileBlock(ref mut e, ref mut ch) => {
                self.visit_exp(e);
                self.visit_chunk(ch);
                None
            },
            &mut Stat::RepeatBlock(ref mut e, ref mut ch) => {
                self.visit_exp(e);
                self.visit_chunk(ch);
                None
            },
            &mut Stat::IfElseBlock(ref mut v, ref mut och) => {
                for &mut (ref mut e, ref mut ch) in v.iter_mut() {
                    self.visit_exp(e);
                    self.visit_chunk(ch);
                }
                if let &mut Some(ref mut och) = och {
                    self.visit_chunk(och);
                }
                None
            },
            &mut Stat::ForRangeBlock(ref mut n, ref mut el, ref mut ch) => {
                self.visit_name(n);
                for e in el.iter_mut() {
                    self.visit_exp(e);
                }
                self.visit_chunk(ch.as_mut());
                None
            },
            &mut Stat::ForInBlock(ref mut nl, ref mut el, ref mut ch) => {
                self.visit_name_list(nl.as_mut());
                self.visit_exp_list(el.as_mut());
                self.visit_chunk(ch.as_mut());
                None
            },
            &mut Stat::FunctionDec(ref mut n, ref mut fb) => {
                self.visit_func_name(n);
                self.visit_func_body(fb.as_mut());
                None
            },
            &mut Stat::LocalFunctionDec(ref mut n, ref mut fb) => {
                self.visit_name(n);
                self.visit_func_body(fb.as_mut());
                None
            },
            &mut Stat::LocalAssign(ref mut nl, ref mut el) => {
                for n in nl.0.iter_mut() {
                    self.visit_name(n);
                }
                if let &mut Some(ref mut el) = el {
                    for e in el.0.iter_mut() {
                        self.visit_exp(e);
                    }
                }
                None
            },
        }
    }
    fn visit_last_stat(&mut self, ls: &mut LastStat) -> Option<T> {
        match ls {
            &mut LastStat::Return(ref mut b) => self.visit_exp_list(b.as_mut()),
            &mut LastStat::Break => None,
        }
    }
    fn visit_chunk(&mut self, ch: &mut Chunk) -> Option<T> {
        for s in ch.0.iter_mut() {
            self.visit_stat(s);
        }
        if let &mut Some(ref mut ls) = &mut ch.1 {
            self.visit_last_stat(ls);
        }
        None
    }
    fn visit_name_and_args(&mut self, naa: &mut NameAndArgs) -> Option<T> {
        if let &mut Some(ref mut s) = &mut naa.0 {
            self.visit_name(s);
        }
        self.visit_args(&mut naa.1);
        None
    }
    fn visit_var_suffix(&mut self, vs: &mut VarSuffix) -> Option<T> {
        match vs {
            &mut VarSuffix::Index(ref mut naas, ref mut e) => {
                for naa in naas.iter_mut() {
                    self.visit_name_and_args(naa);
                }
                self.visit_exp(e);
            },
            &mut VarSuffix::Member(ref mut naas, ref mut s) => {
                for naa in naas.iter_mut() {
                    self.visit_name_and_args(naa);
                }
                self.visit_name(s);
            },
        };
        None
    }
    fn visit_var_or_exp(&mut self, voe: &mut VarOrExp) -> Option<T> {
        match voe {
            &mut VarOrExp::Var(ref mut v) => self.visit_var(v),
            &mut VarOrExp::Exp(ref mut e) => self.visit_exp(e),
        }
    }
}
