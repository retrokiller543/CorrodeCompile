// Purpose: main file for the compiler
// Path: src/main.rs

mod lexer;
mod parser;
mod assymbly_generator;

use assymbly_generator::compile_to_assembly;

use std::fs::File;
use std::io::Read;
use std::path::Path;

fn main() {
    let path = Path::new("dev/test_code.fc");
    let file = File::open(path).unwrap();

    let mut expr = String::new();
    // read file content into string expr
    std::io::BufReader::new(file).read_to_string(&mut expr).unwrap();
    
    // compile expr to assembly
    let result = compile_to_assembly(&expr);

    dbg!(result.clone());

    // write assmebly to file
    std::fs::write("dev/dev.asm", result).unwrap();
}