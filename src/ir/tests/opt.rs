use crate::ir::opt::const_prop::ConstantPropagation;
use crate::ir::opt::dead_block::DeadBlockElimination;
use crate::ir::opt::dead_code::DeadCodeElimination;
use crate::ir::transform::*;
use crate::parser::ASTParser;

#[test]
fn const_prop_simple() {
    let program = include_str!("../../../examples/simple.lang");
    let parser = ASTParser::new();

    let ast = parser.parse(program).unwrap();

    let mut ir = generate_ir(ast);

    for (id, func) in ir.functions.iter_enumerated_mut() {
        if (1..=4).contains(&id.as_usize()) {
            continue;
        }

        ConstantPropagation::execute_pass(func);
    }

    assert_eq!(ir.to_string(), include_str!("../../../outputs/simple_constprop.lasm").replace("\r", ""), "IR repr not updated");
}

#[test]
fn dead_code_simple() {
    let program = include_str!("../../../examples/simple.lang");
    let parser = ASTParser::new();

    let ast = parser.parse(program).unwrap();

    let mut ir = generate_ir(ast);

    for (id, func) in ir.functions.iter_enumerated_mut() {
        if (1..=4).contains(&id.as_usize()) {
            continue;
        }

        ConstantPropagation::execute_pass(func);
        DeadCodeElimination::execute_pass(func);
    }

    assert_eq!(ir.to_string(), include_str!("../../../outputs/simple_deadcode.lasm").replace("\r", ""), "IR repr not updated");
}

#[test]
fn dead_block_simple() {
    let program = include_str!("../../../examples/simple.lang");
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

    assert_eq!(ir.to_string(), include_str!("../../../outputs/simple_deadblock.lasm").replace("\r", ""), "IR repr not updated");
}

#[test]
fn misopt_1() {
    let program = include_str!("../../../examples/misopt-1.lang");
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


    assert_eq!(ir.to_string(), include_str!("../../../outputs/misopt-1.lasm").replace("\r", ""), "IR repr not updated");
}