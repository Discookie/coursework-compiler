

use rustc_index::vec::*;
use iced_x86::code_asm::*;

use crate::ir::types::*;

use super::asm::{ASMResult, ASMFunction};

pub struct ASMEmitterContext {
    pub arg_count: usize,
    pub local_count: usize,
    pub is_aligned: bool
}

impl ASMEmitterContext {
    #[inline]
    pub fn offset_of(&self, local: Local) -> AsmMemoryOperand {
        let local_addr = match local.as_usize() {
            0 => rsp + 8 * (self.local_count - self.arg_count - 1),
            x if x <= self.arg_count && x <= 6 => rsp + 8 * (self.local_count - 1 - self.arg_count + x),
            x if x <= self.arg_count && x > 6 => rsp + 8 * (self.local_count - self.arg_count + x),
            x => rsp + 8 * (self.local_count - 1 - x)
        };

        let align_offset = if self.is_aligned { 0 } else { 8 };

        local_addr + align_offset
    }
}

pub fn emit_ir(ir: IR) -> Result<ASMResult, IcedError> {
    let mut functions = IndexVec::with_capacity(ir.functions.len());

    let mut descriptors: Vec<_> = ir.descriptors.iter().map(|(x, y)| (x.clone(), *y)).collect();
    descriptors.sort_by_key(|(_, id)| *id);

    let descriptors: IndexVec<FunctionId, FnDescriptor> = descriptors.into_iter().map(|(x, _)| x).collect();

    let fn_count = ir.functions.len();

    for (func_id, func) in ir.functions.into_iter_enumerated() {
        let mut asm = CodeAssembler::new(64)?;
    
        let mut fn_names = IndexVec::from_fn_n(|_| asm.create_label(), fn_count);
        
        if (1..=4).contains(&func_id.as_usize()) {
            functions.push(ASMFunction {
                asm,
                descriptor: descriptors[func_id].clone(),
                is_extern: true,
                fn_names,
                bb_names: IndexVec::new()
            });

            continue;
        }

        let bb_names = emit_function(func_id, func, &mut asm, &mut fn_names)?;

        functions.push(ASMFunction {
            asm,
            descriptor: descriptors[func_id].clone(),
            is_extern: false,
            fn_names,
            bb_names
        });
    }

    Ok(ASMResult {
        functions,
        descriptors
    })
}

const IR_REGISTERS: &'static [AsmRegister64] = &[rdi, rsi, rdx, rcx, r8, r9];

pub fn emit_function(func_id: FunctionId, func: Function, asm: &mut CodeAssembler, fn_names: &mut IndexVec<FunctionId, CodeLabel>) -> Result<IndexVec<BasicBlockId, (CodeLabel, usize)>, IcedError> {
    asm.set_label(&mut fn_names[func_id])?;

    asm.push(rbp)?;
    asm.mov(rbp, rsp)?;

    // Push args first, if needed
    for &arg_register in [rdi, rsi, rdx, rcx, r8, r9].iter().take(func.arg_count).rev() {
        asm.push(arg_register)?;
    }

    // Then the return value
    asm.push(0i32)?;

    // Then allocate the registers
    let ctx = ASMEmitterContext {
        arg_count: func.arg_count,
        local_count: func.locals.len(),
        // Alignment shifts by even because of rbp and rsi
        is_aligned: func.locals.len() % 2 == 0
    };

    let reg_count = (ctx.local_count - ctx.arg_count - 1) as i32 + if ctx.is_aligned { 0 } else { 1 };
    asm.sub(rsp, 8 * reg_count)?;

    let mut block_labels = IndexVec::from_fn_n(|_: BasicBlockId| (asm.create_label(), 0), func.body.len());

    let mut phi_ends = IndexVec::from_elem(Vec::new(), &func.body);
    let mut initial_phi = Vec::new();

    for block in func.body.iter() {
        for stmt in block.statements.iter() {
            if let Statement::Phi(target, sources) = stmt {
                for &(source, src_bb) in sources.iter() {
                    if let Some(src_bb) = src_bb {
                        phi_ends[src_bb].push((*target, source));
                    } else {
                        initial_phi.push((*target, source));
                    }
                }
            }
        }
    }

    for (target, source) in initial_phi {
        emit_statement(
            Statement::Copy(target, Value::Local(source)),
            asm,
            &ctx
        )?;
    }

    for (bb, block) in func.body.into_iter_enumerated() {
        asm.set_label(&mut block_labels[bb].0)?;
        block_labels[bb].1 = asm.instructions().len();

        let is_empty_block = block.statements.is_empty() && phi_ends[bb].is_empty();

        for stmt in block.statements {
            emit_statement(stmt, asm, &ctx)?;
        }

        // Phi nodes appended to block ends
        for (target, source) in phi_ends[bb].iter().copied() {
            emit_statement(
                Statement::Copy(target, Value::Local(source)),
                asm,
                &ctx
            )?;
        }

        if let Terminator::FnCall { function: called, args, ret, .. } = &block.terminator {
            // Register arguments
            for (&arg_register, &local) in IR_REGISTERS.iter().zip(args.iter()) {
                asm.mov(arg_register, ctx.offset_of(local))?;
            }

            

            // Alignment before any stack arguments
            let needs_alignment_of = if args.len() > 6 && args.len() % 2 == 1 { 1 } else { 0 };

            if needs_alignment_of > 0 {
                asm.sub(rsp, 8 * needs_alignment_of)?;
            }

            // Stack arguments
            for &local in args.iter().skip(IR_REGISTERS.len()).rev() {
                asm.push(qword_ptr(ctx.offset_of(local)))?;
            }

            asm.call(fn_names[*called])?;

            // Cleanup stack args with alignment
            if args.len() > IR_REGISTERS.len() {
                asm.add(rsp, 8 * (args.len() - IR_REGISTERS.len()) as i32)?;
            }

            // Clean alignment move
            if needs_alignment_of > 0 {
                asm.add(rsp, 8 * needs_alignment_of)?;
            }

            // Return value
            if let Some(local) = ret {
                asm.mov(ctx.offset_of(*local), rax)?;
            }
        }

        match block.terminator {
            Terminator::Goto(target) |
            Terminator::FnCall { next_block: Some(target), .. } => {
                if target != bb + 1 {
                    asm.jmp(block_labels[target].0)?;
                } else if is_empty_block {
                    // Workaround for labeling empty blocks
                    asm.nop()?;
                }
            },
            Terminator::If { cond, then_block, else_block } => {
                asm.mov(al, ctx.offset_of(cond))?;
                asm.test(al, al)?;
                
                match (then_block == bb + 1, else_block == bb + 1) {
                    (true, true) => {},
                    (true, false) => { asm.jz(block_labels[else_block].0)?; }
                    (false, true) => { asm.jnz(block_labels[then_block].0)?; }
                    (false, false) => {
                        asm.jnz(block_labels[then_block].0)?;
                        asm.jmp(block_labels[else_block].0)?;
                    }
                }
            },
            Terminator::FnCall { next_block: None, .. } |
            Terminator::Return => {
                // Allocations
                asm.add(rsp, 8 * reg_count)?;
                asm.pop(rax)?;
                // Arguments passed via registers
                asm.add(rsp, 8 * (ctx.arg_count.min(IR_REGISTERS.len())) as i32)?;

                asm.pop(rbp)?;

                asm.ret()?;
            },
        }
    }

    Ok(block_labels)
}

pub fn emit_statement(stmt: Statement, asm: &mut CodeAssembler, ctx: &ASMEmitterContext) -> Result<(), IcedError> {
    Ok(match stmt {
        Statement::Copy(target, source) => {
            let target_stack = ctx.offset_of(target);
            match source {
                Value::Bool(val) => { asm.mov(target_stack, val as i32)?; },
                Value::Int(val) => {
                    asm.mov(rax, val as i64)?;
                    asm.mov(target_stack, rax)?;
                },
                Value::Local(source) => {
                    asm.mov(rax, ctx.offset_of(source))?;
                    asm.mov(target_stack, rax)?;
                },
            }
        },
        Statement::UnaryOp(target, op, source) => {
            match source {
                Value::Bool(val) => { asm.mov(rax, val as i64)?; }
                Value::Int(val) => { asm.mov(rax, val as i64)?; }
                Value::Local(local) => { asm.mov(rax, ctx.offset_of(local))?; }
            }

            match op {
                UnaryOp::Neg => { asm.neg(rax)?; },
                UnaryOp::Not => { asm.not(rax)?; },
            }

            asm.mov(ctx.offset_of(target), rax)?;
        },
        Statement::BinOp(target, op, left, right) => {
            match left {
                Value::Bool(val) => { asm.mov(rax, val as i64)?; }
                Value::Int(val) => { asm.mov(rax, val as i64)?; }
                Value::Local(local) => { asm.mov(rax, ctx.offset_of(local))?; }
            }
            match right {
                Value::Bool(val) => { asm.mov(rcx, val as i64)?; }
                Value::Int(val) => { asm.mov(rcx, val as i64)?; }
                Value::Local(local) => { asm.mov(rcx, ctx.offset_of(local))?; }
            }

            let target_stack = ctx.offset_of(target);

            match op {
                BinOp::Or |
                BinOp::BinOr => {
                    asm.or(rax, rcx)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::And |
                BinOp::BinAnd => {
                    asm.and(rax, rcx)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::Eq => {
                    asm.cmp(rax, rcx)?;
                    asm.sete(target_stack)?;
                },
                BinOp::NEq => {
                    asm.cmp(rax, rcx)?;
                    asm.setne(target_stack)?;
                },
                BinOp::Lt => {
                    asm.cmp(rax, rcx)?;
                    asm.setl(target_stack)?;
                },
                BinOp::Gt => {
                    asm.cmp(rax, rcx)?;
                    asm.setg(target_stack)?;
                },
                BinOp::LEq => {
                    asm.cmp(rax, rcx)?;
                    asm.setle(target_stack)?;
                },
                BinOp::GEq => {
                    asm.cmp(rax, rcx)?;
                    asm.setge(target_stack)?;
                },
                BinOp::Xor => {
                    asm.xor(rax, rcx)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::Lsh => {
                    asm.shl(rax, cl)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::Rsh => {
                    asm.shr(rax, cl)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::Plus => {
                    asm.add(rax, rcx)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::Minus => {
                    asm.sub(rax, rcx)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::Mul => {
                    asm.xor(rdx, rdx)?;
                    asm.imul(rcx)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::Div => {
                    asm.xor(rdx, rdx)?;
                    asm.idiv(rcx)?;
                    asm.mov(target_stack, rax)?;
                },
                BinOp::Mod => {
                    asm.xor(rdx, rdx)?;
                    asm.idiv(rcx)?;
                    asm.mov(target_stack, rdx)?;
                },
            }
        },
        Statement::Phi(_, _) => { /* ignored */ },
    })
}
