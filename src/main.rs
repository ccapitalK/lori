#[macro_use]
extern crate nom;

use std::fs::File;
use std::io::Read;

mod types;
mod lex;
mod parse;
use types::Parseable;

fn main() {
    let mut data = Vec::new();
    {
        let mut f = File::open("in.lua").unwrap();
        f.read_to_end(&mut data).unwrap();
    }
    let tokens = lex::tokenify_string(data.as_slice()).unwrap();
    let parsed = types::Chunk::parse(&tokens).unwrap();
    println!("{:?}", parsed);
}
