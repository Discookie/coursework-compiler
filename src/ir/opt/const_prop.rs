//! Does constant propagation and unreachable code elimination in one pass, to achieve better optimization.

use std::collections::HashSet;

use rustc_index::vec::*;

use crate::ir::types::*;
use crate::ir::util::graphs::*;

/// A slightly modified version of Sparse Conditional Constant Propagation, so that value reassignments also get changed in the same step.
/// The compiler does not care about 'too old' values, so it's worth doing it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstState {
    /// Top, not decided yet
    Any,
    /// Value, opt can either substitute a constant or another local
    One(Value),
    /// Bot, unoptimizable
    None
}

macro_rules! either {
    ($op:pat, $x:pat, $var:pat) => (($op, $x, $var) | ($op, $var, $x));
    ($x:pat, $var:pat) => (($x, $var) | ($var, $x))
}

fn eval_binop(op: BinOp, left: ConstState, right: ConstState) -> ConstState {
    macro_rules! v {
        ($x:pat) => (ConstState::One($x));
    }

    match (op, left, right) {
        either!(BinOp::Or, v!(Value::Bool(true)), _) => ConstState::One(Value::Bool(true)),
        either!(BinOp::Or, v!(Value::Bool(false)), x) => x,
        (BinOp::Or, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => left,
        
        either!(BinOp::And, v!(Value::Bool(false)), _) => ConstState::One(Value::Bool(false)),
        either!(BinOp::And, v!(Value::Bool(true)), x) => x,
        (BinOp::And, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => left,

        either!(BinOp::Eq, v!(Value::Bool(true)), x)  => x,
        (BinOp::Eq, v!(Value::Bool(false)), v!(Value::Bool(false))) => ConstState::One(Value::Bool(true)),
        (BinOp::Eq, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Bool(a == b)),
        (BinOp::Eq, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Bool(true)),

        either!(BinOp::NEq, v!(Value::Bool(false)), x)  => x,
        (BinOp::NEq, v!(Value::Bool(true)), v!(Value::Bool(true))) => ConstState::One(Value::Bool(false)),
        (BinOp::NEq, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Bool(a != b)),
        (BinOp::NEq, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Bool(false)),

        (BinOp::Lt, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Bool(a < b)),
        (BinOp::Lt, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Bool(false)),

        (BinOp::Gt, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Bool(a > b)),
        (BinOp::Gt, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Bool(false)),

        (BinOp::LEq, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Bool(a <= b)),
        (BinOp::LEq, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Bool(true)),

        (BinOp::GEq, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Bool(a >= b)),
        (BinOp::GEq, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Bool(true)),

        (BinOp::BinOr, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a | b)),
        either!(BinOp::BinOr, v!(Value::Int(a)), _) if a == !0 => ConstState::One(Value::Int(!0)),
        either!(BinOp::BinOr, v!(Value::Int(0)), x) => x,
        (BinOp::BinOr, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => left,

        (BinOp::Xor, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a ^ b)),
        either!(BinOp::Xor, v!(Value::Int(0)), x) => x,
        (BinOp::Xor, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Int(0)),

        (BinOp::BinAnd, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a & b)),
        either!(BinOp::BinAnd, v!(Value::Int(0)), _) => ConstState::One(Value::Int(0)),
        either!(BinOp::BinAnd, v!(Value::Int(a)), x) if a == !0 => x,
        (BinOp::BinAnd, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => left,

        (BinOp::Lsh, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a << b)),
        (BinOp::Lsh, v!(Value::Int(0)), _) => ConstState::One(Value::Int(0)),
        (BinOp::Lsh, x, v!(Value::Int(0))) => x,
        (BinOp::Lsh, _, v!(Value::Int(a))) if !((-(isize::BITS as isize)+1) .. isize::BITS as isize).contains(&a) => ConstState::One(Value::Int(0)),

        (BinOp::Rsh, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a >> b)),
        (BinOp::Rsh, v!(Value::Int(0)), _) => ConstState::One(Value::Int(0)),
        (BinOp::Rsh, x, v!(Value::Int(0))) => x,
        (BinOp::Rsh, _, v!(Value::Int(a))) if !((-(isize::BITS as isize)+1) .. isize::BITS as isize).contains(&a) => ConstState::One(Value::Int(0)),

        (BinOp::Plus, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a + b)),
        either!(BinOp::Plus, v!(Value::Int(0)), x) => x,
        
        (BinOp::Minus, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a - b)),
        (BinOp::Minus, x, v!(Value::Int(0))) => x,
        (BinOp::Minus, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Int(0)),
        
        (BinOp::Mul, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a * b)),
        either!(BinOp::Mul, v!(Value::Int(0)), _) => ConstState::One(Value::Int(0)),
        either!(BinOp::Mul, v!(Value::Int(1)), x) => x,

        (BinOp::Div, _, v!(Value::Int(0))) => {
            eprintln!("Found a confirmed division by zero in the code");
            ConstState::None
        },
        (BinOp::Div, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a / b)),
        (BinOp::Div, x, v!(Value::Int(1))) => x,
        (BinOp::Div, v!(Value::Int(0)), _) => ConstState::One(Value::Int(0)),
        (BinOp::Div, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Int(1)),

        (BinOp::Mod, _, v!(Value::Int(0))) => {
            eprintln!("Found a confirmed modulo by zero in the code");
            ConstState::None
        },
        (BinOp::Mod, v!(Value::Int(a)), v!(Value::Int(b))) => ConstState::One(Value::Int(a % b)),
        (BinOp::Mod, _, v!(Value::Int(1))) => ConstState::One(Value::Int(0)),
        (BinOp::Mod, v!(Value::Int(0)), _) => ConstState::One(Value::Int(0)),
        (BinOp::Mod, v!(Value::Local(x)), v!(Value::Local(y))) if x == y => ConstState::One(Value::Int(0)),
        
        either!(_, ConstState::Any, _) => ConstState::Any,
        _ => ConstState::None
    }
}

pub struct ConstantPropagation {
    local_types: IndexVec<Local, ConstState>,
    blocks_visited: IndexVec<BasicBlockId, bool>,
}

impl ConstantPropagation {
    pub fn execute_pass(func: &mut Function) {
        let const_prop = ConstantPropagation::analyze(func);
        const_prop.replace(func);
    }

    fn analyze(func: &Function) -> ConstantPropagation {
        let ssa = SSAGraph::new_from(func);

        // cfg_processed[To].contains(From)
        let mut cfg_processed = IndexVec::<_, HashSet<BasicBlockId>>::from_elem(HashSet::new(), &func.body);

        let mut cfg_worklist = Vec::<(BasicBlockId, BasicBlockId)>::new();
        // define, and index of use
        let mut ssa_worklist = Vec::<(Local, usize)>::new();

        let mut local_types = IndexVec::from_elem(ConstState::Any, &func.locals);
        let mut term_types = IndexVec::from_elem(ConstState::Any, &func.body);

        cfg_worklist.push((ENTRY_POINT, ENTRY_POINT));

        for i in 0..=4 {
            local_types[Local::new(i)] = ConstState::None;
        }

        macro_rules! eval_prop {
            (early_return $target:expr) => {
                let _: Local = $target;

                if let ConstState::None = local_types[$target] {
                    // Continue should be fine here, as we're always in loops
                    continue;
                }
            };
            (source_state $source:expr) => {{
                let _: Value = $source;

                match $source {
                    Value::Local(local) if local_types[local] != ConstState::None => local_types[local],
                    val => ConstState::One(val)
                }
            }};
            (target_state $target:expr, $target_state:expr) => {{
                let _: Local = $target;
                let _: ConstState = $target_state;

                if local_types[$target] != $target_state {
                    local_types[$target] = $target_state;
                    
                    for idx in 0..ssa.uses($target).len() {
                        ssa_worklist.push(($target, idx));
                    }
                }
            }};
            (phi $target:expr, $source:expr, $block:expr) => {{
                let _: Local = $target;
                let _: &Vec<(Local, BasicBlockId)> = $source;
                let _: BasicBlockId = $block;

                eval_prop!(early_return $target);

                let collection_iter = $source.iter()
                    .filter(|&&(_, bb)| {
                        cfg_processed[bb].contains(&$block)
                    }).map(|&(local, _)| local_types[local]);

                let target_state = collection_iter.fold(
                    ConstState::Any,
                    |current, next| {
                        match (current, next) {
                            (_, ConstState::One(Value::Local(a))) if a == $target => current,
                            (ConstState::Any, ConstState::Any) => ConstState::Any,
                            either!(ConstState::Any, ConstState::One(x)) => ConstState::One(x),
                            (ConstState::One(x), ConstState::One(y)) if x == y => current,
                            (ConstState::One(_), ConstState::One(_)) => ConstState::None,
                            _ => ConstState::None,
                        }
                    }
                );

                eval_prop!(target_state $target, target_state)
            }};
            (stmt $stmt:expr, $block:expr) => {{
                let _: &Statement = $stmt;
                let _: BasicBlockId = $block;

                match $stmt {
                    &Statement::Copy(target, source) => {
                        eval_prop!(early_return target);

                        let target_state = eval_prop!(source_state source);

                        eval_prop!(target_state target, target_state);
                    },
                    &Statement::UnaryOp(target, op, right) => {
                        eval_prop!(early_return target);

                        let target_state = match (eval_prop!(source_state right), op) {
                            (ConstState::One(Value::Bool(b)), UnaryOp::Not) => ConstState::One(Value::Bool(!b)),
                            (ConstState::One(Value::Int(i)), UnaryOp::Not) => ConstState::One(Value::Int(!i)),
                            (ConstState::One(Value::Int(i)), UnaryOp::Neg) => ConstState::One(Value::Int(-i)),
                            (ConstState::One(_), _) => ConstState::None,
                            (ConstState::Any, _) => ConstState::Any,
                            (ConstState::None, _) => ConstState::None
                        };

                        eval_prop!(target_state target, target_state);
                    },
                    &Statement::BinOp(target, op, left, right) => {
                        eval_prop!(early_return target);

                        let target_state = eval_binop(op, eval_prop!(source_state left), eval_prop!(source_state right));

                        eval_prop!(target_state target, target_state);
                    },
                    Statement::Phi(source, target) => eval_prop!(phi *source, target, $block),
                }
            }};
            (term $term:expr, $block:expr) => {{
                let _: &Terminator = $term;
                let _: BasicBlockId = $block;

                match $term {
                    &Terminator::Goto(next) => {
                        if (!cfg_processed[$block].contains(&next)) {
                            cfg_worklist.push(($block, next));
                        }
                    },
                    Terminator::FnCall { ret, next_block, .. } => {
                        if let &Some(target) = ret {
                            eval_prop!(early_return target);

                            let target_state = ConstState::None;

                            eval_prop!(target_state target, target_state);
                        }

                        if let &Some(next) = next_block {
                            if (!cfg_processed[$block].contains(&next)) {
                                cfg_worklist.push(($block, next));
                            }
                        }
                    },
                    &Terminator::If{ cond, then_block, else_block } => {
                        if let ConstState::None = term_types[$block] {
                            break;
                        }

                        if term_types[$block] != local_types[cond] {
                            term_types[$block] = local_types[cond];

                            match term_types[$block] {
                                ConstState::One(Value::Bool(true)) => {
                                    if (!cfg_processed[$block].contains(&then_block)) {
                                        cfg_worklist.push(($block, then_block));
                                    }
                                },
                                ConstState::One(Value::Bool(false)) => {
                                    if (!cfg_processed[$block].contains(&else_block)) {
                                        cfg_worklist.push(($block, else_block));
                                    }
                                },
                                ConstState::One(Value::Local(_)) |
                                ConstState::None => {
                                    if (!cfg_processed[$block].contains(&then_block)) {
                                        cfg_worklist.push(($block, then_block));
                                    }
                                    if (!cfg_processed[$block].contains(&else_block)) {
                                        cfg_worklist.push(($block, else_block));
                                    }
                                },
                                ConstState::Any => {},
                                ConstState::One(Value::Int(_)) => panic!("Invalid type in If terminator")
                            }
                        }
                    }
                    Terminator::Return => {}
                }
            }}
        }

        while !cfg_worklist.is_empty() || !ssa_worklist.is_empty() {
            if let Some((cfg_from, cfg_to)) = cfg_worklist.pop() {
                let do_cond_evals = cfg_processed[cfg_to].is_empty();
                if cfg_processed[cfg_from].insert(cfg_to) {
                    if do_cond_evals {
                        for stmt in func.body[cfg_to].statements.iter() {
                            eval_prop!(stmt stmt, cfg_to);
                        }

                        // Workaround for early return in terminstor
                        let mut looped = true;
                        while looped {
                            looped = false;
                            eval_prop!(term &func.body[cfg_to].terminator, cfg_to);
                        }
                    } else {
                        for stmt in func.body[cfg_to].statements.iter() {
                            if let Statement::Phi(local, from_vec) = stmt {
                                eval_prop!(phi *local, from_vec, cfg_to);
                            }
                        }
                    }
                }
            }

            if let Some((local, idx)) = ssa_worklist.pop() {
                let (bb, use_idx) = ssa.uses(local)[idx];

                if cfg_processed[bb].is_empty() {
                    continue;
                }

                // Workaround for early return in terminstor
                let mut looped = true;
                while looped {
                    looped = false;
                    if let Some(stmt_idx) = use_idx {
                        let stmt = &func.body[bb].statements[stmt_idx];
                        eval_prop!(stmt stmt, bb);
                    } else {
                        let term = &func.body[bb].terminator;
                        eval_prop!(term term, bb);
                    }
                }
            }
        }

        let blocks_visited = cfg_processed.into_iter().map(|set| !set.is_empty()).collect();

        ConstantPropagation {
            local_types,
            blocks_visited
        }
    }

    fn replace(self, func: &mut Function) {
        macro_rules! replace {
            ($param:expr) => {
                let _: &mut Value = $param;

                if let Value::Local(local) = $param {
                    if let ConstState::One(val) = self.local_types[*local] {
                        *$param = val;
                    }
                }
            }
        }

        for (bb, block) in func.body.iter_enumerated_mut() {
            if !self.blocks_visited[bb] {
                *block = BasicBlock::new(Terminator::Return);
                block.unreachable = true;
                continue;
            }

            for stmt in block.statements.iter_mut() {
                match stmt {
                    &mut Statement::Copy(target, _) |
                    &mut Statement::UnaryOp(target, _, _) |
                    &mut Statement::BinOp(target, _, _, _) |
                    &mut Statement::Phi(target, _) => {
                        if let ConstState::One(val) = self.local_types[target] {
                            *stmt = Statement::Copy(target, val);
                            continue;
                        }
                    }
                }

                match stmt {
                    Statement::Copy(_, source) |
                    Statement::UnaryOp(_, _, source) => {
                        replace!(source);
                    },
                    Statement::BinOp(_, _, left, right) => {
                        replace!(left);
                        replace!(right);
                    }
                    Statement::Phi(_, locals) => {
                        locals.retain(|(_, bb)| self.blocks_visited[*bb]);
                    }
                }
            }

            match &mut block.terminator {
                Terminator::If { cond, then_block, else_block } => {
                    match self.local_types[*cond] {
                        ConstState::One(Value::Bool(true)) => {
                            block.terminator = Terminator::Goto(*then_block);
                        },
                        ConstState::One(Value::Bool(false)) => {
                            block.terminator = Terminator::Goto(*else_block);
                        }
                        ConstState::One(Value::Local(local)) => {
                            *cond = local;
                        }
                        _ => {}
                    }
                },
                Terminator::FnCall {..} |
                Terminator::Goto(_) |
                Terminator::Return => {},
            }
        }
    }
}