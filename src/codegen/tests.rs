use crate::codegen::emit::emit_ir;
use crate::ir::opt::const_prop::ConstantPropagation;
use crate::ir::opt::dead_block::DeadBlockElimination;
use crate::ir::opt::dead_code::DeadCodeElimination;
use crate::ir::transform::*;
use crate::parser::ASTParser;

#[test]
fn gen_simple_unoptimized() {
    let program = include_str!("../../examples/simple.lang");
    let parser = ASTParser::new();

    let ast = parser.parse(program).unwrap();

    let ir = generate_ir(ast);

    let mut buf = Vec::new();
    let asm = emit_ir(ir).expect("Failed to generate asm");
    asm.write_nasm(&mut buf).expect("Failed to generate textform asm");
    
    let nasm_output = String::from_utf8(buf).expect("Generated invalid encoding");

    assert_eq!(nasm_output, include_str!("../../outputs/gen_simple.nasm").replace("\r", ""), "IR repr not updated");
}

#[test]
fn gen_simple_optimized() {
    let program = include_str!("../../examples/simple.lang");
    let parser = ASTParser::new();

    let ast = parser.parse(program).unwrap();

    let mut ir = generate_ir(ast);

    for (id, func) in ir.functions.iter_enumerated_mut() {
        if (1..=4).contains(&id.as_usize()) {
            continue;
        }

        ConstantPropagation::execute_pass(func);
        DeadCodeElimination::execute_pass(func);
        DeadBlockElimination::execute_pass(func);
    }

    let mut buf = Vec::new();
    let asm = emit_ir(ir).expect("Failed to generate asm");
    asm.write_nasm(&mut buf).expect("Failed to generate textform asm");
    
    let nasm_output = String::from_utf8(buf).expect("Generated invalid encoding");

    assert_eq!(nasm_output, include_str!("../../outputs/gen_simple_opt.nasm").replace("\r", ""), "IR repr not updated");
}
