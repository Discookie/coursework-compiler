use crate::ir::transform::*;
use crate::parser::ASTParser;


#[test]
fn transform_simple() {
    let program = include_str!("../../../examples/simple.lang");
    let parser = ASTParser::new();

    let ast = parser.parse(program).unwrap();

    let ir = generate_ir(ast);

    assert_eq!(ir.to_string(), include_str!("../../../outputs/simple.lasm").replace("\r", ""), "IR repr not updated");
}