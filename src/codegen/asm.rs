use std::io;

use rustc_index::vec::*;
use iced_x86::{code_asm::*, NasmFormatter, Formatter};

use crate::ir::types::*;

pub struct ASMResult {
    pub functions: IndexVec<FunctionId, ASMFunction>,
    pub descriptors: IndexVec<FunctionId, FnDescriptor>
}

impl ASMResult {
    pub fn write_nasm<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        writeln!(w, "section .text")?;
        writeln!(w)?;
        // List externs at the top
        for function in self.functions.iter() {
            if function.is_extern {
                function.write_nasm(w, &self)?;
            }
        }

        writeln!(w)?;
        
        // Hardcoded _start entrypoint
        writeln!(w, "\
            global _start\n\
            _start:\n\
            call main\n\
            mov rbx, rax\n\
            mov rax, 60 ; sysexit\n\
            xor rdi, rdi\n\
            syscall\
        ")?;
        
        writeln!(w)?;

        // And then the rest
        for function in self.functions.iter() {
            if !function.is_extern {
                function.write_nasm(w, &self)?;
            }
        }

        Ok(())
    }
}

pub struct ASMFunction {
    pub asm: CodeAssembler,
    pub descriptor: FnDescriptor,
    pub is_extern: bool,
    pub fn_names: IndexVec<FunctionId, CodeLabel>,
    // Label, location in instr. offset
    pub bb_names: IndexVec<BasicBlockId, (CodeLabel, usize)>,
}

impl ASMFunction {
    pub fn write_nasm<W: io::Write>(&self, w: &mut W, parent: &ASMResult) -> io::Result<()> {
        if self.is_extern {
            writeln!(w, "extern {}", self.descriptor.mangled_name())?;
            return Ok(());
        }

        let mut formatter = NasmFormatter::new();

        writeln!(w, "global {}", self.descriptor.mangled_name())?;
        writeln!(w, "{}:", self.descriptor.mangled_name())?;

        let mut bb_iter = self.bb_names.iter_enumerated().peekable();

        let mut mnemonic = String::new();
        let mut formatted = String::new();

        // Assuming sorted
        for (idx, instruction) in self.asm.instructions().iter().enumerate() {
            if let Some(&(bb, &(_, loc))) = bb_iter.peek() {
                if idx == loc {
                    writeln!(w)?;
                    writeln!(w, "._{}:", bb)?;
                    bb_iter.next();
                }
            }

            mnemonic.clear();
            formatter.format_mnemonic(&instruction, &mut mnemonic);
            formatted.clear();
            formatter.format(instruction, &mut formatted);

            // Function call
            if formatted.starts_with("call") {
                let fn_idx = instruction.near_branch64() as usize - 1;
                let descriptor = &parent.descriptors[fn_idx.into()];

                writeln!(w, "{} {}", mnemonic, descriptor.mangled_name())?;
            } else if formatted.starts_with("j") {
                let bb_id = instruction.near_branch64() as usize - parent.descriptors.len() - 1;

                writeln!(w, "{} ._{}", mnemonic, BasicBlockId::new(bb_id))?;
            } else {
                writeln!(w, "{}", formatted)?;
            }
        }

        writeln!(w)?;
        writeln!(w, "align 16")?;
        writeln!(w)?;

        Ok(())
    }
}

