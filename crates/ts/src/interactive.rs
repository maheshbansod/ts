//! Interactive mode
use std::{
    error::Error,
    io::{stdin, stdout, Write},
};

use crate::{checker::Checker, parser::Parser, tokenizer::Tokenizer};

pub fn interactive() -> Result<(), Box<dyn Error>> {
    loop {
        show_prompt();
        stdout().flush()?;
        let line = read_line()?;
        if line.trim_end() == "exit" {
            break;
        } else if line.trim().is_empty() {
            continue;
        }
        let tokenizer = Tokenizer::new(&line);
        let parser = Parser::new(tokenizer);
        match parser.parse() {
            Ok((tree, errors)) => {
                if errors.is_empty() {
                    // no errors, let's type check?
                    let checker = Checker::new(&tree);
                    let (errors, scope) = checker.check();
                    if !errors.is_empty() {
                        println!("Errors:");
                        for error in &errors {
                            println!("Error: {error:?}");
                        }
                    }

                    println!("Type information: ");
                    for (i, t) in scope.symbols() {
                        println!("i: {i:?}, type: {t:?}");
                    }
                } else {
                    println!("Errors:");
                    for error in errors {
                        println!("Error: {error}");
                    }
                }
            }
            Err(e) => println!("Unhandled exception occurred: {e}\nThis could be a bug in tscheck"),
        }
    }
    Ok(())
}

fn show_prompt() {
    print!("> ");
}

fn read_line() -> Result<String, Box<dyn Error>> {
    let mut buffer = String::new();
    stdin().read_line(&mut buffer)?;
    Ok(buffer)
}
