use std::marker::PhantomData;

use rustc_index::vec::*;

use crate::ir::types::*;

pub struct CFGGraph<'a> {
    edges: IndexVec<BasicBlockId, Vec<BasicBlockId>>,
    back_edges: IndexVec<BasicBlockId, Vec<BasicBlockId>>,
    _marker: PhantomData<&'a Function>
}

impl<'a> CFGGraph<'a> {
    pub fn new_from(func: &Function) -> CFGGraph {
        let mut edges = IndexVec::from_elem(Vec::new(), &func.body);
        let mut back_edges = IndexVec::from_elem(Vec::new(), &func.body);

        let mut add_edge = |from, to| {
            edges[from].push(to);
            back_edges[to].push(from);
        };

        for (current, bb) in func.body.iter_enumerated() {
            match bb.terminator {
                Terminator::Goto(next) |
                Terminator::FnCall { next_block: Some(next), .. } => add_edge(current, next),
                Terminator::If { then_block, else_block, .. } => {
                    add_edge(current, then_block);
                    add_edge(current, else_block);
                },
                _ => {},
            }
        }

        CFGGraph { edges, back_edges, _marker: PhantomData }
    }

    pub fn from_block(&self, from: BasicBlockId) -> &Vec<BasicBlockId> {
        &self.edges[from]
    }

    pub fn to_block(&self, to: BasicBlockId) -> &Vec<BasicBlockId> {
        &self.back_edges[to]
    }

}

pub type SSAGraphItem = (BasicBlockId, Option<usize>);

pub struct SSAGraph<'a> {
    // (X, None) means the terminator
    define: IndexVec<Local, Option<SSAGraphItem>>,
    uses: IndexVec<Local, Vec<SSAGraphItem>>,
    _marker: PhantomData<&'a Function>
}

impl<'a> SSAGraph<'a> {
    pub fn new_from(func: &Function) -> SSAGraph {
        let mut define = IndexVec::from_elem(None, &func.locals);
        let mut uses = IndexVec::from_elem(Vec::new(), &func.locals);

        for (current, bb) in func.body.iter_enumerated() {
            for (idx, stmt) in bb.statements.iter().enumerate() {
                let loc = (current, Some(idx));

                match stmt {
                    Statement::Copy(to, from) |
                    Statement::UnaryOp(to, _, from) => {
                        define[*to] = Some(loc);

                        if let Value::Local(from_loc) = from {
                            uses[*from_loc].push(loc);
                        }
                    },
                    Statement::BinOp(to, _, left, right) => {
                        define[*to] = Some(loc);

                        if let Value::Local(left_loc) = left {
                            uses[*left_loc].push(loc);
                        }

                        if let Value::Local(right_loc) = right {
                            uses[*right_loc].push(loc);
                        }
                    },
                    Statement::Phi(to, from_vec) => {
                        define[*to] = Some(loc);

                        for (from, _) in from_vec.iter() {
                            uses[*from].push(loc);
                        }
                    }
                }
            }

            match &bb.terminator {
                Terminator::FnCall { args, ret, .. } => {
                    if let Some(ret_loc) = ret {
                        define[*ret_loc] = Some((current, None));
                    }

                    for arg_loc in args.iter() {
                        uses[*arg_loc].push((current, None));
                    }
                },
                Terminator::If { cond, .. } => {
                    uses[*cond].push((current, None));
                },
                _ => {},
            }
        }

        SSAGraph { define, uses, _marker: PhantomData }
    }

    pub fn define(&self, local: Local) -> SSAGraphItem {
        self.define[local].unwrap()
    }

    pub fn uses(&self, local: Local) -> &Vec<SSAGraphItem> {
        &self.uses[local]
    }
}
