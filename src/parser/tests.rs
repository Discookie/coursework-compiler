mod ast {
    use crate::parser::ASTParser;

    #[test]
    fn ast_loop_simple() {
        let program = include_str!("../../examples/simple.lang");
        let parser = ASTParser::new();

        let ast = parser.parse(program).unwrap();

        assert_eq!(program.replace("\r", ""), ast.to_string())
    }
}

mod typecheck {
    use crate::parser::ASTParser;
    use crate::parser::typecheck::*;

    #[test]
    fn type_check_simple() {
        let program = include_str!("../../examples/simple.lang");
        let parser = ASTParser::new();

        let ast = parser.parse(program).unwrap();

        let mut tcx = TypeResolverCtxt::new();

        if let Err(err) = ast.resolve_type(&mut tcx) {
            panic!("failed to resolve type: {}", err);
        };

        assert_eq!(tcx.stack_height(), 0, "Stack height invalid");
    }
}