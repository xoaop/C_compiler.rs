use crate::asm_ast::{self, AssemblyAst};
use std::fs::File;
use std::io::Write;



pub fn generate_assembly(assembly_ast: &AssemblyAst) {
    let file_path = "output.asm";
    let mut file = match File::create(file_path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to create file: {}", e);
            return;
        }
    };

    let result = generate_at_program(&assembly_ast.program);

    if let Err(e) = file.write(result.as_bytes()) {
        eprintln!("Failed to write to file: {}", e);
    }

}



fn generate_at_program(program: &asm_ast::Program) -> String  {
    let mut result = String::new();

    result.push_str(&generate_at_function(&program.function_definition));
    
    result.push_str(".section .note.GNU-stack,\"\",@progbits\n");

    return result;
}


fn generate_at_function(function: &asm_ast::Function) -> String {
    let mut result = String::new();

    result.push_str(format!("\t.globl {}\n{}:\n", function.identifier.name, function.identifier.name).as_str()); 

    for inst in function.instructions.iter() {
        result.push_str(format!("\t{}\n", inst).as_str());
    }

    return result;
}
