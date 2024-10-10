use std::{error::Error, fs::read_to_string};

use checker::Checker;
use parser::Parser;
use tokenizer::Tokenizer;

mod checker;
mod interactive;
mod parser;
mod tokenizer;

fn main() -> Result<(), Box<dyn Error>> {
    let args = std::env::args();
    if args.len() == 1 {
        interactive::interactive()
    } else {
        for arg in args.skip(1) {
            let file_name = arg;
            println!("Loading file '{file_name}'...");
            let code = read_to_string(file_name)?; // todo use buffer
            println!("Loaded code into memory ✅");
            let tokenizer = Tokenizer::new(&code);
            let parser = Parser::new(tokenizer);
            println!("Parsing...");
            match parser.parse() {
                Ok((tree, errors)) => {
                    if errors.is_empty() {
                        println!("No parser errors ✅");
                    } else {
                        println!("❌ Parser errors: {errors:?}");
                    }
                    let checker = Checker::new(&tree);
                    println!("Type checking...");
                    let (errors, types) = checker.check();
                    if errors.is_empty() {
                        println!("No checker errors ✅");
                    } else {
                        println!("❌ Type checking Errors: {:?}", errors);
                    }
                    println!("Types: {:?}", types);
                }
                Err(err) => {
                    println!("❌ Error while parsing");
                    println!("{err:?}");
                }
            }
        }
        Ok(())
    }
}
