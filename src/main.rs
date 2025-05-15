mod lexer;
mod parser;
mod ast;
mod asmt;
mod asmgen;
mod tacky;

use std::env;

use asmgen::generate_assembly;


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <path_to_file>", args[0]);
        std::process::exit(1);
    }

    let path = &args[1];
    let path = r"C:\Users\xoaop\Desktop\Rust\c_complier\test\foo.c";
    println!("Reading file: {}", path);
    // Open the file

    let tokens = lexer::get_token_list(path).expect("获取token列表失败");

    for token in tokens.iter() {
        println!("{:?}", token);
    }

    let mut parser = parser::Parser::new(tokens);

    let ast = parser.parse();

    println!("{:#?}", &ast);

    let tacky_p = tacky::emit_program(&ast.root);

    println!("{:#?}", &tacky_p);

    let asmt = asmt::emit_program(&tacky_p);

    println!("{:#?}", &asmt);

    let asmt = asmt::Asmt { program: asmt };

    generate_assembly(&asmt);


}
