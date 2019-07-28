#[macro_use]
extern crate nom;
extern crate ordered_float;

use std::fs::File;
use std::io::Read;

mod ast_types;
mod interpreter;
mod parse;

use ast_types::ASTVisitor;
use interpreter::Interpreter;

fn main() {
    let mut data = Vec::new();
    {
        let mut f = File::open("in.lua").unwrap();
        f.read_to_end(&mut data).unwrap();
    }
    let tokens = parse::tokenify_string(data.as_slice()).unwrap();
    let mut parsed = parse::parse_chunk(&tokens).unwrap();
    let mut interpreter = Interpreter::new();
    interpreter.visit_chunk(&mut parsed);
}
