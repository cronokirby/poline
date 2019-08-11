use std::fs::File;
use std::io;
use std::io::prelude::*;

mod parser;
use parser::collect_errors_and_parse;
mod interpreter;
use interpreter::interpret;
mod simplifier;
use simplifier::simplify;

fn main() -> io::Result<()> {
    let mut args = std::env::args();
    args.next();
    let file_name = args.next().unwrap();
    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("Parsing...");
    let syntax =
        collect_errors_and_parse(&contents).unwrap_or_else(|e| panic!("Error parsing: {:?}", e));
    println!("Simplifying...");
    let program = simplify(syntax).unwrap_or_else(|e| panic!("Error simplifying: {:?}", e));
    println!("Interpreting..\n");
    interpret(program);
    Ok(())
}
