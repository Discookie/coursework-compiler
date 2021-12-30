use std::mem;

use rustc_index::vec::*;

use crate::ir::{types::*, util::{traversal::{ReversePostorder, Preorder}, graphs::CFGGraph}};

// Uses BasicBlock.unreachable for removed blocks.
pub struct DeadBlockElimination {
    forward_merged_blocks: IndexVec<BasicBlockId, Option<BasicBlockId>>,
    phi_replacements: IndexVec<Local, Option<Vec<(Local, BasicBlockId)>>>
}

impl DeadBlockElimination {
    pub fn execute_pass(func: &mut Function) {
        let dead_block = DeadBlockElimination::analyze(func);
        dead_block.replace(func);
    }

    fn analyze(func: &Function) -> DeadBlockElimination {
        let mut forward_merged_blocks = IndexVec::from_elem(None, &func.body);
        let mut phi_replacements = IndexVec::from_elem(None, &func.locals);

        let cfg = CFGGraph::new_from(func);

        for (bb, block) in func.body.iter_enumerated() {
            if let &Terminator::Goto(suc) = &block.terminator {
                if cfg.to_block(suc).len() == 1 ||  block.statements.len() == 0 {
                    forward_merged_blocks[bb] = Some(suc);
                }
            }
        }

        let mut block_backrefs = IndexVec::from_fn_n(|idx: BasicBlockId| vec![idx], func.body.len());

        for bb in ReversePostorder::new(&func.body, ENTRY_POINT, None) {
            if forward_merged_blocks[bb].is_some() {
                block_backrefs[bb] = cfg.to_block(bb).iter()
                    .flat_map(|pred| block_backrefs[*pred].iter().copied())
                    .collect()
            }
        }

        for block in func.body.iter() {
            for stmt in block.statements.iter() {
                if let Statement::Phi(target, sources) = stmt {
                    let new_sources = sources.iter()
                        .flat_map(|(local, source)| {
                            block_backrefs[*source].iter().map(|pred| (*local, *pred))
                        }).collect();

                    if sources != &new_sources {
                        phi_replacements[*target] = Some(new_sources);
                    }
                }
            }
        }

        DeadBlockElimination {
            forward_merged_blocks,
            phi_replacements
        }
    }

    fn replace(self, func: &mut Function) {
        for block in func.body.iter_mut() {
            for stmt in block.statements.iter_mut() {
                if let Statement::Phi(target, _) = stmt {
                    if let Some(new_sources) = &self.phi_replacements[*target] {
                        *stmt = Statement::Phi(*target, new_sources.clone());
                    }
                }
            }
        }

        let rpo: Vec<_> = ReversePostorder::new(&func.body, ENTRY_POINT, None).collect();

        // Instructions in bodies
        for bb in rpo {
            if let Some(target) = self.forward_merged_blocks[bb] {
                let mut target_body = func.body[bb].statements.clone();

                if target_body.len() == 0 {
                    continue;
                }

                mem::swap(&mut func.body[target].statements, &mut target_body);
                func.body[target].statements.extend(target_body.into_iter());
            }
        }

        let mut reverse_preorder: Vec<_> = Preorder::new(&func.body, ENTRY_POINT, None).collect();
        reverse_preorder.reverse();

        let mut block_fwdrefs = IndexVec::from_fn_n(|idx: BasicBlockId| idx, func.body.len());

        // Jump locations
        for bb in reverse_preorder {
            if let Some(target) = self.forward_merged_blocks[bb] {
                func.body[bb].unreachable = true;
                block_fwdrefs[bb] = block_fwdrefs[target];
            }
        }

        // Rewrite jumps
        for bb in func.body.iter_mut() {
            let mut term = Terminator::Return;
            mem::swap(&mut term, &mut bb.terminator);
            bb.terminator = match term {
                Terminator::Goto(target)
                    => Terminator::Goto(block_fwdrefs[target]),
                Terminator::FnCall { function, args, ret, next_block: Some(target) }
                    => Terminator::FnCall { function, args, ret, next_block: Some(block_fwdrefs[target]) },
                Terminator::If { cond, then_block, else_block }
                    => Terminator::If { cond, then_block: block_fwdrefs[then_block], else_block: block_fwdrefs[else_block] },
                x => x,
            }
        }

        
        // Get new ids of unreachable blocks
        let new_ids = {
            let mut new_ids: IndexVec<BasicBlockId, _> = func.body.iter_enumerated()
            .filter_map(|(bb, block)| if !block.unreachable { Some(bb) } else { None })
            .collect();

            // Swap the new entrypoint into place before proceeding
            let entry_idx = *new_ids.iter().enumerate()
                .find(|&(_, bb)| *bb == block_fwdrefs[ENTRY_POINT])
                .expect("No entrypoint after dead block elimination").1;

            new_ids.swap(entry_idx, ENTRY_POINT);
            new_ids
        };

        let old_ids = {
            let mut old_ids = IndexVec::from_fn_n(|idx: BasicBlockId| idx, func.body.len());

            for (new_id, old_id) in new_ids.iter_enumerated() {
                old_ids[*old_id] = new_id;
            }

            old_ids
        };

        // And finally remove the unreachable blocks and update refs
        let mut temp_body = IndexVec::new();
        mem::swap(&mut temp_body, &mut func.body);
        func.body = temp_body.into_iter().filter(|block| !block.unreachable).collect();

        for block in func.body.iter_mut() {
            for stmt in block.statements.iter_mut() {
                if let Statement::Phi(_, sources) = stmt {
                    for (_, block) in sources {
                        *block = old_ids[*block];
                    }
                }
            }

            match &mut block.terminator {
                Terminator::Goto(target) |
                Terminator::FnCall { next_block: Some(target), .. } => {
                    *target = old_ids[*target]
                },
                Terminator::If { then_block, else_block, .. } => {
                    *then_block = old_ids[*then_block];
                    *else_block = old_ids[*else_block];
                },
                _ => {},
            }
        }

    }
}