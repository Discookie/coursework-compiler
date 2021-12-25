use std::{collections::HashMap, mem};

use rustc_index::vec::*;

use super::{
    types::*,
    util::traversal::*
};
use crate::parser::{
    ast,
    typecheck::*
};

#[derive(Debug, Clone)]
pub struct TransformCtxt<'a> {
    pub variable_map: HashMap<String, Option<Local>>,
    pub function_map: &'a HashMap<FnDescriptor, (ast::Function, FunctionId)>,
}

pub fn convert_ty(ty: Option<ast::Typename>) -> Typename {
    match ty {
        Some(ast::Typename::Bool) => Typename::Bool,
        Some(ast::Typename::Int) => Typename::Int,
        None => Typename::None,
    }
}

pub fn convert_unary(op: ast::UnaryOp) -> UnaryOp {
    match op {
        ast::UnaryOp::Neg => UnaryOp::Neg,
        ast::UnaryOp::Not => UnaryOp::Not,
        ast::UnaryOp::Pos => panic!("+ unary op not removed"),
    }
}

pub fn convert_binary(op: ast::BinOp) -> BinOp {
    match op {
        ast::BinOp::Or => BinOp::Or,
        ast::BinOp::And => BinOp::And,
        ast::BinOp::Eq => BinOp::Eq,
        ast::BinOp::NEq => BinOp::NEq,
        ast::BinOp::Lt => BinOp::Lt,
        ast::BinOp::Gt => BinOp::Gt,
        ast::BinOp::LEq => BinOp::LEq,
        ast::BinOp::GEq => BinOp::GEq,
        ast::BinOp::BinOr => BinOp::BinOr,
        ast::BinOp::Xor => BinOp::Xor,
        ast::BinOp::BinAnd => BinOp::BinAnd,
        ast::BinOp::Lsh => BinOp::Lsh,
        ast::BinOp::Rsh => BinOp::Rsh,
        ast::BinOp::Plus => BinOp::Plus,
        ast::BinOp::Minus => BinOp::Minus,
        ast::BinOp::Mul => BinOp::Mul,
        ast::BinOp::Div => BinOp::Div,
        ast::BinOp::Mod => BinOp::Mod
    }
}

pub fn convert_assign(op: ast::AssignOp) -> Option<BinOp> {
    match op {
        ast::AssignOp::Equal => None,
        ast::AssignOp::BinOr => Some(BinOp::BinOr),
        ast::AssignOp::Xor => Some(BinOp::Xor),
        ast::AssignOp::BinAnd => Some(BinOp::BinAnd),
        ast::AssignOp::Lsh => Some(BinOp::Lsh),
        ast::AssignOp::Rsh => Some(BinOp::Rsh),
        ast::AssignOp::Add => Some(BinOp::Plus),
        ast::AssignOp::Sub => Some(BinOp::Minus),
        ast::AssignOp::Mul => Some(BinOp::Mul),
        ast::AssignOp::Div => Some(BinOp::Div),
        ast::AssignOp::Mod => Some(BinOp::Mod)
    }
}

pub fn convert_descriptor(desc: ast::FnDescriptor) -> FnDescriptor {
    FnDescriptor {
        name: desc.name,
        args: desc.args.into_iter().map(|x| convert_ty(Some(x))).collect(),
    }
}

/// Result will be stored in .1
pub fn generate_expr(expr: ast::Expr, tcx: &TransformCtxt, function: &mut Function, mut block: BasicBlockId) -> (BasicBlockId, Local) {
    macro_rules! block {
        () => (function.body[block])
    }

    macro_rules! unpack {
        (let $($vars:ident),+ = $expr:ident ($arg:expr)) => {
            let (new_block, $($vars),+) = $expr($arg, tcx, function, block);
            block = new_block;
        }
    }

    match expr {
        ast::Expr::Paren { expr } => {
            generate_expr(*expr, tcx, function, block)
        }

        ast::Expr::FnCall { name, args } => {
            let mut locals = Vec::with_capacity(args.len());
            for arg in args {
                unpack!(let new_local = generate_expr(arg));
                locals.push(new_local);
            }

            let args: Vec<Typename> = locals.iter().map(|&x| function.locals[x]).collect();

            let (fn_data, fn_id) = &tcx.function_map[&FnDescriptor { name, args }];

            let ret_ty = convert_ty(fn_data.ret);
            let ret = function.locals.push(ret_ty);

            let mut term = Terminator::FnCall {
                function: *fn_id,
                args: locals,
                ret: Some(ret),
                next_block: Some(function.body.next_index())
            };

            mem::swap(&mut term, &mut block!().terminator);
            
            block = function.body.push(BasicBlock::new(term));
            (block, ret)
        }

        ast::Expr::UnaryOp { op, right } => {
            unpack!(let new_local = generate_expr(*right));

            if let ast::UnaryOp::Pos = op {
                return (block, new_local);
            }

            let local_ty = function.locals[new_local];
            let target_local = function.locals.push(local_ty);

            block!().statements.push(Statement::UnaryOp(
                target_local,
                convert_unary(op),
                Value::Local(new_local)
            ));

            (block, target_local)
        }

        ast::Expr::BinOp { left, op, right } => {
            unpack!(let left_local = generate_expr(*left));
            unpack!(let right_local = generate_expr(*right));

            let target_type = match op.creates_type() {
                TypeCreation::Certain(x) => convert_ty(x),
                TypeCreation::Same => function.locals[left_local]
            };

            let target_local = function.locals.push(target_type);

            block!().statements.push(Statement::BinOp(
                target_local,
                convert_binary(op),
                Value::Local(left_local),
                Value::Local(right_local)
            ));

            (block, target_local)
        }

        ast::Expr::Const { value } => {
            let (target_value, target_type) = match value {
                ast::Value::Bool(b) => (Value::Bool(b), Typename::Bool),
                ast::Value::Int(b) => (Value::Int(b), Typename::Int),
            };
            let target_local = function.locals.push(target_type);

            block!().statements.push(Statement::Copy(
                target_local,
                target_value
            ));

            (block, target_local)
        }

        ast::Expr::Var { name } => {
            (block, tcx.variable_map[&name].expect("unassigned var"))
        }
    }
}

/// Returns true if block returns
pub fn generate_block(body: Vec<ast::Statement>, tcx: &mut TransformCtxt, function: &mut Function, mut block: BasicBlockId) -> (BasicBlockId, bool) {
    macro_rules! unpack {
        (let $($vars:ident),+ = $expr:ident ($arg:expr)) => {
            let (new_block, $($vars),+) = $expr($arg, tcx, function, block);
            block = new_block;
        }
    }

    for stmt in body {
        unpack!(let stmt_returns = generate_stmt(stmt));

        if stmt_returns {
            return (block, true);
        }
    }

    (block, false)
}

/// Returns true if stmt is a return
pub fn generate_stmt(stmt: ast::Statement, tcx: &mut TransformCtxt, function: &mut Function, mut block: BasicBlockId) -> (BasicBlockId, bool) {
    macro_rules! block {
        () => (function.body[block])
    }

    macro_rules! unpack {
        (let $($vars:ident),+ = $expr:ident ($arg:expr)) => {
            let (new_block, $($vars),+) = $expr($arg, tcx, function, block);
            block = new_block;
        }
    }

    match stmt {
        ast::Statement::Block { body } => {
            generate_block(body, tcx, function, block)
        }

        ast::Statement::Return { expr } => { 
            unpack!(let new_local = generate_expr(expr));

            block!().statements.push(Statement::Copy(
                Local::new(0),
                Value::Local(new_local)
            ));

            block!().terminator = Terminator::Return;

            (block, true)
        }

        ast::Statement::FnCall { name, args } => {
            let mut locals = Vec::with_capacity(args.len());
            for arg in args {
                unpack!(let new_local = generate_expr(arg));
                locals.push(new_local);
            }

            let args: Vec<Typename> = locals.iter().map(|&x| function.locals[x]).collect();

            let (_, fn_id) = &tcx.function_map[&FnDescriptor { name, args }];

            let mut term = Terminator::FnCall {
                function: *fn_id,
                args: locals,
                ret: None,
                next_block: Some(function.body.next_index())
            };

            mem::swap(&mut term, &mut block!().terminator);

            block = function.body.push(BasicBlock::new(term));
            (block, false)
        }

        ast::Statement::Define { name, value, .. } => {
            unpack!(let new_local = generate_expr(value));

            tcx.variable_map.insert(name, Some(new_local));

            (block, false)
        }

        ast::Statement::Assign { name, op, value } => {
            unpack!(let right_local = generate_expr(value));

            if let Some(target_op) = convert_assign(op) {
                let left_local = tcx.variable_map[&name].expect("unassigned var");

                let local_ty = function.locals[right_local];
                let target_local = function.locals.push(local_ty);
                
                block!().statements.push(Statement::BinOp(
                    target_local,
                    target_op,
                    Value::Local(left_local),
                    Value::Local(right_local)
                ));

                tcx.variable_map.insert(name, Some(target_local));
            } else {
                tcx.variable_map.insert(name, Some(right_local));
            }

            (block, false)
        }

        ast::Statement::If { cond, body, else_body} => {
            unpack!(let cond_local = generate_expr(cond));

            let mut term = Terminator::Return;

            mem::swap(&mut term, &mut block!().terminator);

            // Then block
            let mut then_tcx = tcx.clone();
            // Placeholder term
            let then_block = function.body.push(BasicBlock::new(Terminator::Return));
            let (end_then_block, then_returns) = generate_block(body, &mut then_tcx, function, then_block);

            let mut else_tcx = tcx.clone();

            let (else_blocks, else_returns) = if let Some(else_body) = else_body {
                // Placeholder term
                let else_block = function.body.push(BasicBlock::new(Terminator::Return));

                let (end_else_block, else_returns) = generate_block(else_body, &mut else_tcx, function, else_block);
                (Some((else_block, end_else_block)), else_returns)
            } else {
                (None, false)
            };

            match (then_returns, else_returns) {
                // Function not continuing
                (then_returns, true) => {
                    tcx.variable_map = then_tcx.variable_map;

                    let (else_block, _end_else_block) = else_blocks.unwrap();

                    block!().terminator = Terminator::If {
                        cond: cond_local,
                        then_block,
                        else_block
                    };

                    (end_then_block, then_returns)
                },

                (true, false) => {

                    let (else_block, end_else_block) = if let Some(blocks) = else_blocks {
                        tcx.variable_map = else_tcx.variable_map;

                        blocks
                    } else {
                        let final_block = function.body.push(BasicBlock::new(term));

                        (final_block, final_block)
                    };

                    block!().terminator = Terminator::If {
                        cond: cond_local,
                        then_block,
                        else_block
                    };

                    (end_else_block, false)
                },

                (false, false) => {
                    let final_block = function.body.push(BasicBlock::new(term));

                    function.body[end_then_block].terminator = Terminator::Goto(final_block);

                    let else_block = if let Some((else_block, end_else_block)) = else_blocks {
                        function.body[end_else_block].terminator = Terminator::Goto(final_block);

                        else_block
                    } else {
                        final_block
                    };

                    block!().terminator = Terminator::If {
                        cond: cond_local,
                        then_block,
                        else_block
                    };

                    block = final_block;

                    let difference_map = {
                        let mut difference_map = HashMap::new();

                        for (k, overall_v) in tcx.variable_map.iter() {
                            if let None = overall_v {
                                continue;
                            }
                        
                            let vals = (
                                then_tcx.variable_map[k],
                                else_tcx.variable_map[k]
                            );
                        
                            match vals {
                                (Some(l), Some(r)) if l != r => {
                                    difference_map.insert(k.clone(), vec![l, r]);
                                },
                                (Some(_), Some(_)) => continue,
                                _ => panic!("asymmetric var assignment")
                            }
                        };

                        difference_map
                    };

                    for (k, vals) in difference_map {
                        let new_v = function.locals.push(function.locals[vals[0]]);

                        block!().statements.push(Statement::Phi(new_v, vals));
                        tcx.variable_map.insert(k, Some(new_v));
                    }

                    (block, false)
                }
            }
        }

        ast::Statement::While { cond, body } => {
            // placeholder term
            let cond_block = function.body.push(BasicBlock::new(Terminator::Return));

            let mut term = Terminator::Goto(cond_block);
            mem::swap(&mut term, &mut block!().terminator);

            block = cond_block;
            
            let while_block = function.body.push(BasicBlock::new(Terminator::Goto(block)));
            let mut while_tcx = tcx.clone();

            let (end_while_block, while_returns) = generate_block(body, &mut while_tcx, function, while_block);

            if while_returns {
                block!().terminator = Terminator::Goto(while_block);

                return (end_while_block, true);
            }

            function.body[end_while_block].terminator = Terminator::Goto(cond_block);

            let mut difference_map = HashMap::new();

            for (k, overall_v) in tcx.variable_map.iter() {
                if let None = overall_v {
                    continue;
                }
            
                let vals = (
                    tcx.variable_map[k],
                    while_tcx.variable_map[k]
                );
            
                match vals {
                    (Some(l), Some(r)) if l != r => {
                        difference_map.insert(k.clone(), (l, r));
                    },
                    _ => continue
                }
            };

            for (_, (left, right)) in difference_map.iter_mut() {
                let new_local = function.locals.push(function.locals[*left]);

                block!().statements.push(Statement::Phi (
                    new_local,
                    vec![*left, *right]
                ));

                *right = new_local;
            }

            let mut replace_vals = IndexVec::<Local, Option<Local>>::new();

            for &(left, new) in difference_map.values() {
                replace_vals.ensure_contains_elem(left, || None);
                replace_vals[left] = Some(new);
            }

            replace_variable_in_blocks(
                function,
                while_block,
                end_while_block,
                replace_vals
            );

            let (new_block, cond_local) = generate_expr(cond, tcx, function, block);
            block = new_block;

            let final_block = function.body.push(BasicBlock::new(term));

            block!().terminator = Terminator::If {
                cond: cond_local,
                then_block: while_block,
                else_block: final_block
            };

            (final_block, false)
        }

        ast::Statement::For { name, from, to, mut body } => {
            unpack!(let for_local = generate_expr(from));
            unpack!(let until_local = generate_expr(to));

            tcx.variable_map.insert(name.clone(), Some(for_local));
            tcx.variable_map.insert(format!("{}$$until", name), Some(until_local));

            body.push(ast::Statement::Assign {
                name: name.clone(),
                op: ast::AssignOp::Add,
                value: ast::Expr::Const {
                    value: ast::Value::Int(1)
                },
            });

            let dummy_stmt = ast::Statement::While {
                cond: ast::Expr::BinOp {
                    left: Box::new(ast::Expr::Var { name: name.clone() }),
                    op: ast::BinOp::Lt,
                    right: Box::new(ast::Expr::Var { name: format!("{}$$until", name) }),
                },
                body,
            };

            generate_stmt(dummy_stmt, tcx, function, block)
        }

        ast::Statement::ForEq { name, from, mut to, body } => {
            to = ast::Expr::BinOp {
                left: Box::new(to),
                op: ast::BinOp::Plus,
                right: Box::new(ast::Expr::Const { value: ast::Value::Int(1) } ),
            };

            generate_stmt(ast::Statement::ForEq {
                name,
                from,
                to,
                body,
            }, tcx, function, block)
        }
    }
}

pub fn generate_function(ast_function: ast::Function, function_map: &HashMap<FnDescriptor, (ast::Function, FunctionId)>) -> Function {
    let mut function = Function {
        body: IndexVec::new(),
        locals: IndexVec::with_capacity(ast_function.args.len() + 1),
        arg_count: ast_function.args.len(),
    };

    let mut tcx = TransformCtxt {
        variable_map: HashMap::new(),
        function_map,
    };

    function.locals.push(convert_ty(ast_function.ret));

    for ast::TypedVar { name, typename } in ast_function.args {
        let local = function.locals.push(convert_ty(Some(typename)));

        tcx.variable_map.insert(name, Some(local));
    }

    let start = function.body.push(BasicBlock::new(Terminator::Return));

    generate_block(ast_function.body, &mut tcx, &mut function, start);

    function
}

pub fn generate_ir(ast: ast::AST) -> IR {
    let mut functions = IndexVec::<FunctionId, Option<Function>>::new();
    let function_map = {
        let mut function_map = HashMap::new();

        for func in ast.contents {
            function_map.insert(
                convert_descriptor(func.descriptor()),
                (
                    func,
                    functions.push(None)
                )
            );
        }

        function_map
    };

    for (_, (func, id)) in function_map.iter() {
        functions[*id] = Some(generate_function(func.clone(), &function_map));
    }

    let functions = functions.into_iter()
        .map(|func| func.expect("Fn generation missed a fn"))
        .collect();
    let descriptors = function_map.into_iter()
        .map(|(descriptor, (_, id))| (descriptor, id))
        .collect();

    IR {
        descriptors,
        functions
    }
}

/// End block inclusive. Replacement (from, to)
fn replace_variable_in_blocks(
    function: &mut Function,
    start_block: BasicBlockId, end_block: BasicBlockId,
    replacement: IndexVec<Local, Option<Local>>
) {
    let order: Vec<BasicBlockId> = preorder(&function.body, start_block, Some(end_block)).collect();
    macro_rules! replace {
        (local: $val:expr) => {
            if let Some(to) = replacement[*$val] {
                *$val = to;
            }
        };
        (value: $val:expr) => {
            if let Value::Local(val) = $val {
                replace!(local: val);
            }
        }
    }

    for block in order {
        macro_rules! block {
            () => (function.body[block])
        }

        for stmt in block!().statements.iter_mut() {
            match stmt {
                Statement::Copy(local, value) |
                Statement::UnaryOp(local, _, value) => {
                    replace!(local: local);
                    replace!(value: value);
                },
                Statement::BinOp(local, _, left, right) => {
                    replace!(local: local);
                    replace!(value: left);
                    replace!(value: right);
                },
                Statement::Phi(local, values) => {
                    replace!(local: local);

                    for loc in values.iter_mut() {
                        replace!(local: loc);
                    }
                }
            }
        }
    }
}
