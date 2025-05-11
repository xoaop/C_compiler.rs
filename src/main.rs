mod lexer;
mod parser;
mod ast;
mod asm_ast;

use std::env;
use asm_ast::trans_ast_to_asmast;
use lexer::get_token_list;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <path_to_file>", args[0]);
        std::process::exit(1);
    }

    let path = &args[1];
    println!("Reading file: {}", path);
    // Open the file

    let tokens = get_token_list(path).expect("获取token列表失败");

    for token in tokens.iter() {
        println!("{:?}", token);
    }

    let mut parser = Parser::new(tokens);

    let ast = parser.parse();

    println!("{:#?}", &ast);

    let assast = trans_ast_to_asmast(&ast);

    println!("{:#?}", &assast);


    

}
