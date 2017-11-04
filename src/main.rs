#[macro_use]
extern crate nom;

use std::fs::File;
use std::io::Read;

mod lex;

fn main() {
    let mut data = Vec::new();
    {
        let mut f = File::open("in.lua").unwrap();
        f.read_to_end(&mut data).unwrap();
    }
    let (_, tokens) = lex::parse_buffer(&data[..]).unwrap();
    println!("{:?}", tokens);
}
