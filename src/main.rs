use parser::Parser;
use tokenizer::Tokenizer;

mod parser;
mod tokenizer;

fn main() {
    let code = "
let x = 1;
let y = 2;
console.log(x + y);
";
    let tokenizer = Tokenizer::new(code);
    let parser = Parser::new(tokenizer);
    let _ = parser.parse();
}
