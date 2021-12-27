use rustc_index::{
    bit_set::*,
    vec::*
};

use crate::ir::types::*;

pub struct Preorder<'a> {
    body: &'a IndexVec<BasicBlockId, BasicBlock>,
    end: Option<BasicBlockId>,
    visited: BitSet<BasicBlockId>,
    worklist: Vec<BasicBlockId>
}

impl<'a> Preorder<'a> {
    pub fn new(body: &IndexVec<BasicBlockId, BasicBlock>, start: BasicBlockId, end: Option<BasicBlockId>) -> Preorder {
        Preorder {
            body,
            end,
            visited: BitSet::new_empty(body.len()),
            worklist: vec![start]
        }
    }
}

pub fn preorder<'a>(body: &'a IndexVec<BasicBlockId, BasicBlock>, start: BasicBlockId, end: Option<BasicBlockId>) -> Preorder<'a> {
    Preorder::new(body, start, end)
}

impl<'a> Iterator for Preorder<'a> {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<BasicBlockId> {
        while let Some(current) = self.worklist.pop() {
            if !self.visited.insert(current) {
                continue;
            }

            match &self.body[current].terminator {
                Terminator::Goto(next) |
                Terminator::FnCall { next_block: Some(next), .. } 
                    => self.worklist.push(*next),
                Terminator::If { then_block, else_block, .. }
                    => self.worklist.extend([*then_block, *else_block]),
                _ => ()
            }

            if Some(current) == self.end {
                self.worklist.clear();
            }

            return Some(current);
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.body.len() - self.visited.count()))
    }
}

pub struct Postorder<'a> {
    body: &'a IndexVec<BasicBlockId, BasicBlock>,
    end: Option<BasicBlockId>,
    visited: BitSet<BasicBlockId>,
    worklist: Vec<BasicBlockId>
}

impl<'a> Postorder<'a> {
    pub fn new(body: &IndexVec<BasicBlockId, BasicBlock>, start: BasicBlockId, end: Option<BasicBlockId>) -> Postorder {
        Postorder {
            body,
            end,
            visited: BitSet::new_empty(body.len()),
            worklist: vec![start]
        }
    }
}

pub fn postorder<'a>(body: &'a IndexVec<BasicBlockId, BasicBlock>, start: BasicBlockId, end: Option<BasicBlockId>) -> Postorder<'a> {
    Postorder::new(body, start, end)
}

impl<'a> Iterator for Postorder<'a> {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<BasicBlockId> {
        while let Some(current) = self.worklist.last().cloned() {
            if Some(current) == self.end {
                self.worklist.pop();
                return Some(current);
            }

            // Add successors, if they're not visited or the last
            let added_successors = match &self.body[current].terminator {
                Terminator::Goto(next) |
                Terminator::FnCall { next_block: Some(next), .. } 
                    if self.visited.insert(*next) => {
                        self.worklist.push(*next);
                        true
                    }
                Terminator::If { then_block, else_block, .. } => {
                    let then_added = self.visited.insert(*then_block);
                    if then_added {
                        self.worklist.push(*then_block);
                    }

                    let else_added = self.visited.insert(*else_block);
                    if else_added {
                        self.worklist.push(*else_block);
                    }
                
                    then_added || else_added
                },
                _ => false
            };

            if !added_successors {
                self.worklist.pop();
                return Some(current);
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.worklist.len(), Some(self.body.len() - self.visited.count()))
    }
}

pub struct ReversePostorder<'a> {
    _body: &'a IndexVec<BasicBlockId, BasicBlock>,
    _end: Option<BasicBlockId>,
    postorder: Vec<BasicBlockId>
}

impl<'a> ReversePostorder<'a> {
    pub fn new(body: &IndexVec<BasicBlockId, BasicBlock>, start: BasicBlockId, end: Option<BasicBlockId>) -> ReversePostorder {
        ReversePostorder {
            _body: body,
            _end: end,
            postorder: Postorder::new(body, start, end).collect()
        }
    }
}

pub fn reverse_postorder<'a>(body: &'a IndexVec<BasicBlockId, BasicBlock>, start: BasicBlockId, end: Option<BasicBlockId>) -> ReversePostorder<'a> {
    ReversePostorder::new(body, start, end)
}

impl<'a> Iterator for ReversePostorder<'a> {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<BasicBlockId> {
        self.postorder.pop()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.postorder.len(), Some(self.postorder.len()))
    }
}

impl<'a> ExactSizeIterator for ReversePostorder<'a> {
    fn len(&self) -> usize {
        self.postorder.len()
    }
}
