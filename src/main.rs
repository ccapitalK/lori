#[macro_use]
extern crate nom;
extern crate ordered_float;

use std::fs::File;
use std::io::Read;

mod types;
mod parse;

fn main() {
    let mut data = Vec::new();
    {
        let mut f = File::open("in.lua").unwrap();
        f.read_to_end(&mut data).unwrap();
    }
    let tokens = parse::tokenify_string(data.as_slice()).unwrap();
    let parsed = parse::parse_chunk(&tokens).unwrap();
}
