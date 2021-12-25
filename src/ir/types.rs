use std::collections::HashMap;

use derive_more::Display;
use rustc_index::{newtype_index, vec::*};

newtype_index!(
    #[derive(Display)]
    #[display(fmt = "_{}", self._raw)]
    pub struct Local {
        const RETURN_PLACE = 0
    }
);

newtype_index!(
    #[derive(Display)]
    #[display(fmt = "fn{}", self._raw)]
    pub struct FunctionId {
        const MAIN_FUNCTION = 0
    }
);

newtype_index!(
    #[derive(Display)]
    #[display(fmt = "bb{}", self._raw)]
    pub struct BasicBlockId {
        const ENTRY_POINT = 0
    }
);

pub struct IR {
    pub descriptors: HashMap<FnDescriptor, Function>,
    pub functions: IndexVec<FunctionId, Function>
}

#[derive(Debug)]
pub struct Function {
    pub body: IndexVec<BasicBlockId, BasicBlock>,
    pub locals: IndexVec<Local, Typename>,
    pub arg_count: usize
}

#[derive(Clone, Debug, Display, PartialEq, Eq, Hash)]
#[display(
    fmt = "Fn({}, ({}))",
    name,
    "format_comma(args)",
)]
pub struct FnDescriptor {
    pub name: String,
    pub args: Vec<Typename>,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Terminator
}

impl BasicBlock {
    pub fn new(terminator: Terminator) -> BasicBlock {
        BasicBlock {
            statements: Vec::new(),
            terminator
        }
    }

    
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
pub enum Typename {
    #[display(fmt = "int")] Int,
    #[display(fmt = "bool")] Bool,
    #[display(fmt = "none")] None
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Bool(bool),
    Int(isize),
    Local(Local),
}

#[derive(Debug, Display, Clone)]
pub enum Statement {
    #[display(fmt = "{} = {}", _0, _1)]
    Copy(Local, Value),
    #[display(fmt = "{} = {}{}", _0, _1, _2)]
    UnaryOp(Local, UnaryOp, Value),
    #[display(fmt = "{} = {} {} {}", _0, _1, _2, _3)]
    BinOp(Local, BinOp, Value, Value),
    #[display(fmt = "{} <- phi({})", _0, "format_comma(_1)")]
    Phi(Local, Vec<Local>)
}

#[derive(Debug, Display)]
pub enum Terminator {
    #[display(fmt="goto {}", _0)]
    Goto (BasicBlockId),
    #[display(
        fmt="call {}({}){}",
        function,
        "format_comma(args)",
        "if let Some(ret_inner) = ret { format!(\", {}\", ret_inner) } else { String::new() }",
    )]
    FnCall {
        function: FunctionId,
        args: Vec<Local>,
        ret: Option<Local>,
        next_block: Option<BasicBlockId>
    },
    #[display(fmt="if {}, {}, {}", cond, then_block, else_block)]
    If {
        cond: Local,
        then_block: BasicBlockId,
        else_block: BasicBlockId
    },
    /// Returned value is Place #0
    #[display(fmt="ret")]
    Return,

}

#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum UnaryOp {
    #[display(fmt = "-")] Neg,
    #[display(fmt = "!")] Not,
}

#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum BinOp {
    #[display(fmt = "||")] Or,
    #[display(fmt = "&&")] And,

    #[display(fmt = "==")] Eq,
    #[display(fmt = "!=")] NEq,
    #[display(fmt = "<")] Lt,
    #[display(fmt = ">")] Gt,
    #[display(fmt = "<=")] LEq,
    #[display(fmt = ">=")] GEq,

    #[display(fmt = "|")] BinOr,
    #[display(fmt = "^")] Xor,
    #[display(fmt = "&")] BinAnd,
    #[display(fmt = "<<")] Lsh,
    #[display(fmt = ">>")] Rsh,

    #[display(fmt = "+")] Plus,
    #[display(fmt = "-")] Minus,
    #[display(fmt = "*")] Mul,
    #[display(fmt = "/")] Div,
    #[display(fmt = "%")] Mod
}

pub struct IRCtxt {
    pub functions: Vec<Function>
}

fn format_comma<T: std::fmt::Display>(comma: &Vec<T>) -> String {
    comma.iter()
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}
