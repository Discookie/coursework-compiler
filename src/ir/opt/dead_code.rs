use rustc_index::{vec::*, bit_set::BitSet};

use crate::ir::{types::*, util::{traversal::ReversePostorder, graphs::SSAGraph}};

// A mark-and-sweep dead code elimination on SSA.
pub struct DeadCodeElimination {
    locals_visited: BitSet<Local>
}

impl DeadCodeElimination {
    pub fn execute_pass(func: &mut Function) {
        let dead_code = DeadCodeElimination::analyze(func);
        dead_code.replace(func);
    }

    fn analyze(func: &Function) -> DeadCodeElimination {
        let mut locals_visited = BitSet::new_empty(func.locals.len());

        for idx in 1..=func.arg_count {
            locals_visited.insert(Local::new(idx));
        }

        let ssa = SSAGraph::new_from(func);
        let mut worklist = Vec::new();

        macro_rules! mark {
            ($local:expr) => {
                let _: Local = $local;

                if !locals_visited.contains($local) {
                    eprintln!("Marking {}", $local);
                    worklist.push($local);
                    locals_visited.insert($local);
                }
            }
        }

        for block in ReversePostorder::new(&func.body, ENTRY_POINT, None) {
            for stmt in &func.body[block].statements {
                match stmt {
                    Statement::Copy(target, _) |
                    Statement::UnaryOp(target, _, _) |
                    Statement::BinOp(target, _, _, _) |
                    Statement::Phi(target, _) if *target == RETURN_PLACE => {
                        mark!(RETURN_PLACE);
                    },
                    _ => {}
                }
            }

            match &func.body[block].terminator {
                Terminator::FnCall { args, .. } => {
                    for arg in args.iter() {
                        mark!(*arg);
                    }
                },
                Terminator::If { cond, .. } => {
                    mark!(*cond);
                },
                Terminator::Goto(_) |
                Terminator::Return => {},
            }
        }

        while let Some(local) = worklist.pop() {
            eprintln!("Popping {}", local);
            let (block, def_idx) = ssa.define(local);

            if let Some(idx) = def_idx {
                match &func.body[block].statements[idx] {
                    Statement::Copy(_, source) |
                    Statement::UnaryOp(_, _, source) => {
                        if let &Value::Local(local) = source {
                            mark!(local);
                        }
                    },
                    Statement::BinOp(_, _, left, right) => {
                        if let &Value::Local(local) = left {
                            mark!(local);
                        }
                        if let &Value::Local(local) = right {
                            mark!(local);
                        }
                    },
                    Statement::Phi(_, sources) => {
                        for source in sources.iter() {
                            mark!(source.0);
                        }
                    },
                }
            } else {
                if let &Terminator::FnCall { ret: Some(ret), .. } = &func.body[block].terminator {
                    mark!(ret);
                } else {
                    continue;
                }
            }
        }

        DeadCodeElimination { locals_visited }
    }

    fn replace(self, func: &mut Function) {
        for block in func.body.iter_mut() {
            block.statements.retain(|stmt| {
                match stmt {
                    &Statement::Copy(target, _) |
                    &Statement::UnaryOp(target, _, _) |
                    &Statement::BinOp(target, _, _, _) |
                    &Statement::Phi(target, _) => {
                        self.locals_visited.contains(target)
                    }
                }
            });

            if let Terminator::FnCall { ret, .. } = &mut block.terminator {
                if let Some(target) = ret {
                    if !self.locals_visited.contains(*target) {
                        *ret = None;
                    }
                }
            }
        }
    }
}
