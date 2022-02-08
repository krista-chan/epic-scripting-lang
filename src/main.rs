use scripting_lang::parser::ast::TopLevel;

fn main() {
    let tl = include_str!("../test.file").parse::<TopLevel>().unwrap();
    // dbg!(tl);
}
