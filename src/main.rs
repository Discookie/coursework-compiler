#![feature(min_specialization)]

use std::fs::{File, read_to_string};
use std::io::Write;
use std::path::PathBuf;

use clap::{Parser, ArgEnum};
use codegen::emit::emit_ir;
use ir::opt::const_prop::ConstantPropagation;
use ir::opt::dead_block::DeadBlockElimination;
use ir::opt::dead_code::DeadCodeElimination;
use ir::transform::generate_ir;
use parser::{ASTParser, typecheck::{TypeResolverCtxt, TypeResolverVisitor}};

pub mod parser;
pub mod ir;
pub mod codegen;

#[derive(Parser)]
#[clap(name = "coursework-compiler")]
struct Args {
    #[clap(index(1), help = "Path to the input .lang file")]
    input: String,
    #[clap(short, long, help = "Path of the output .nasm file")]
    output: Option<String>,
    #[clap(long, help = "Disable optimizations")]
    no_opt: bool,
    #[clap(long, arg_enum, help = "Output internal artifacts")]
    emit: Option<EmitOptions>
}

#[derive(ArgEnum, Clone)]
enum EmitOptions {
    Ast,
    Ir,
    IrOpt
}

fn main() {
    let args = Args::parse();

    let in_path = PathBuf::from(&args.input);

    let out_path = match &args.output {
        Some(output) => PathBuf::from(output),
        None => {
            let mut out_path = in_path.clone();
            out_path.set_extension("nasm");
            out_path
        },
    };

    // Parsing
    let ast = {
        let in_program = read_to_string(in_path).expect("Failed to read input file");

        let parser = ASTParser::new();
        let ast = parser.parse(&in_program).expect("Failed to parse program");

        let mut tcx = TypeResolverCtxt::new();

        ast.resolve_type(&mut tcx).expect("Failed to resolve type");

        if let Some(EmitOptions::Ast) = args.emit {
            let mut ast_path = out_path.clone();
            ast_path.set_extension("last");

            let mut ast_file = File::create(ast_path).expect("Failed to open AST output");
            write!(ast_file, "{}", ast).expect("Failed to write AST output");

            return;
        }

        ast
    };

    let ir = {
        let mut ir = generate_ir(ast);

        if let Some(EmitOptions::Ir) = args.emit {
            let mut ir_path = out_path.clone();
            ir_path.set_extension("lasm");

            let mut ir_file = File::create(ir_path).expect("Failed to open IR output");
            write!(ir_file, "{}", ir).expect("Failed to write IR output");

            return;
        }

        if !args.no_opt {
            for (id, func) in ir.functions.iter_enumerated_mut() {
                if (1..=4).contains(&id.as_usize()) {
                    continue;
                }

                ConstantPropagation::execute_pass(func);
                DeadCodeElimination::execute_pass(func);
                DeadBlockElimination::execute_pass(func);
            }
        }

        if let Some(EmitOptions::IrOpt) = args.emit {
            let mut ir_path = out_path.clone();
            ir_path.set_extension("opt.lasm");

            let mut ir_file = File::create(ir_path).expect("Failed to open optimized IR output");
            write!(ir_file, "{}", ir).expect("Failed to write optimized IR output");

            return;
        }

        ir
    };

    let nasm = {
        let mut buf = Vec::new();
        let asm = emit_ir(ir).expect("Failed to generate ASM");
        
        asm.write_nasm(&mut buf).expect("Failed to generate textform ASM");
        String::from_utf8(buf).expect("Failed to generate textform ASM")
    };

    let mut out_file = File::create(out_path).expect("Failed to open ASM output");
    write!(out_file, "{}", nasm).expect("Failed to write ASM output");
}
