use std::error::Error;

mod interactive;
mod parser;
mod string_utils;
mod tokenizer;

fn main() -> Result<(), Box<dyn Error>> {
    interactive::interactive()
}
