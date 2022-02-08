use parser::ast::TopLevel;
use parser::error::Diagnostic;

mod parser;

fn main() {
    let tl = include_str!("../test.file").parse::<TopLevel>().unwrap();
    // dbg!(tl);
}
