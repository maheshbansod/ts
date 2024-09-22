use std::error::Error;

mod interactive;
mod parser;
mod tokenizer;

fn main() -> Result<(), Box<dyn Error>> {
    interactive::interactive()
}
