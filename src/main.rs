use std::fs::File;
use std::io;
use std::io::prelude::*;

mod parser;
use parser::collect_errors_and_parse;

fn main() -> io::Result<()> {
    let mut args = std::env::args();
    args.next();
    let file_name = args.next().unwrap();
    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let result = collect_errors_and_parse(&contents);
    match result {
        Err(e) => println!("Error parsing: {:?}", e),
        Ok(syn) => println!("Parse result: {:?}", syn),
    }
    Ok(())
}
