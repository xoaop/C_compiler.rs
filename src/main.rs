mod asmgen;
mod asmt;
mod lex;
mod parse;
mod tacky;
mod global;



use asmgen::generate_assembly;
use asmt::Asmt;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <path_to_file>", args[0]);
        std::process::exit(1);
    }

    let path = &args[1];

    
    let tokens = lex::lexer::get_token_list(path).expect("获取token列表失败");
    
    println!("{:#?}", &tokens);
    
    let mut variable_context = global::VariableContext::new();
    let mut parser = parse::parser::Parser::new(tokens, &mut variable_context);
    
    let ast = parser.parse();
    
    println!("{:#?}", &ast);
    
    let mut tackilizer = tacky::Tackilizer::new(parser.variable_context);

    let tacky_p = tackilizer.emit_program(&ast.root);

    println!("{:#?}", &tacky_p);

    let mut asmt = Asmt::new();
    let p = asmt.emit_program(&tacky_p);

    println!("{:#?}", &p);

    generate_assembly(&p);
}
