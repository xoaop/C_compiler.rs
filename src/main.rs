mod asmgen;
mod asmt;
mod lex;
mod parse;
mod tacky;
mod global;
mod symbol;


use asmt::Asmt;
use std::env;

use crate::asmgen::emit_asm;



const ALL_STAGE: [&str; 5] = [
    "lex",
    "parse",
    "tacky",
    "asmt",
    "asmgen"
];


fn main() {
    
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <path_to_file> [-s <stage name>] [-p (print all content)]", args[0]);
        std::process::exit(0);
    }

    let mut print_all = false;
    let mut final_stage = String::new();
    let mut path = String::new();


    //参数解析
    let mut idx = 0;
    while idx < args.len() {
        match args[idx].as_str() {
            "-p" => {
                print_all = true;
            }

            "-s" => {
                idx += 1;
                
                final_stage = args.get(idx).expect("-s 参数缺失").clone();

                if !ALL_STAGE.contains(&final_stage.as_str()) {
                    panic!("无效的阶段名称: '{}'. 有效的阶段有: {:?}", final_stage, ALL_STAGE);
                }
            }

            _ => {
                path = args[idx].clone();
            }
        }
        idx += 1;
    }
    
    let mut stage_idx = 0;

    //每一阶段的打印和检查是否结束
    let mut stage_msg = |content: &dyn std::fmt::Debug| {
        if print_all || final_stage == ALL_STAGE[stage_idx] {
            println!("{:#?}", content);
        }

        if final_stage == ALL_STAGE[stage_idx] {
            std::process::exit(0);
        }

        stage_idx += 1;
    };


    let tokens = lex::lexer::get_token_list(&path).expect("获取token列表失败");
    
    stage_msg(&tokens);

    let mut variable_context = global::VariableContext::new();
    let mut symbol_table = std::collections::HashMap::<String, symbol::SymbolInfo>::new();

    let mut parser = parse::parser::Parser::new(tokens, &mut variable_context, &mut symbol_table);
    
    let ast = parser.parse();
    
    stage_msg(&ast);
    
    let mut tackilizer = tacky::Tackilizer::new(parser.variable_context, parser.symbol_table);

    let tacky_p = tackilizer.emit_program(&ast.root);

    stage_msg(&tacky_p);

    let mut asmt = Asmt::new(tackilizer.symbol_table);
    let p = asmt.emit_program(&tacky_p);

    stage_msg(&p);

    emit_asm(&p, &path);
}
