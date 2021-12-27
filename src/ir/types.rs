use std::{collections::HashMap, fmt};

use derive_more::Display;
use rustc_index::{newtype_index, vec::*};

newtype_index!(
    pub struct Local {
        const RETURN_PLACE = 0
    }
);

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.private)
    }
}

newtype_index!(
    pub struct FunctionId {
        const MAIN_FUNCTION = 0
    }
);

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn{}", self.private)
    }
}

newtype_index!(
    pub struct BasicBlockId {
        const ENTRY_POINT = 0
    }
);

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.private)
    }
}

#[derive(Debug)]
pub struct IR {
    pub descriptors: HashMap<FnDescriptor, FunctionId>,
    pub functions: IndexVec<FunctionId, Function>
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "descriptors:")?;

        let mut keys: Vec<_> = self.descriptors.iter().collect();
        keys.sort_by_key(|(_, &x)| x);

        for (descriptor, id) in keys {
            writeln!(f, "    {}: {}", id, descriptor)?;
        }

        writeln!(f)?;
        writeln!(f, "functions:")?;
        writeln!(f)?;

        for (id, func) in self.functions.iter_enumerated() {
            writeln!(f, "{}: {}", id, func)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Function {
    pub body: IndexVec<BasicBlockId, BasicBlock>,
    pub locals: IndexVec<Local, Typename>,
    pub arg_count: usize
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "fn({} args) -> {} {{", self.arg_count, self.locals[Local::new(0)])?;

        writeln!(f, "locals:")?;
        for (local, ty) in self.locals.iter_enumerated() {
            writeln!(f, "    {}: {}", local, ty)?;
        }
        writeln!(f)?;

        for (id, block) in self.body.iter_enumerated() {
            writeln!(f, "{}:", id)?;
            writeln!(f, "{}", block)?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
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
    pub terminator: Terminator,
    pub unreachable: bool
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            writeln!(f, "    {}", stmt)?;
        }

        writeln!(f, "    {}", self.terminator)?;

        Ok(())
    }
}

impl BasicBlock {
    pub fn new(terminator: Terminator) -> BasicBlock {
        BasicBlock {
            statements: Vec::new(),
            terminator,
            unreachable: false
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
    #[display(fmt = "{} = {} {} {}", _0, _2, _1, _3)]
    BinOp(Local, BinOp, Value, Value),
    #[display(
        fmt = "{} <- phi({})    [{}]",
        _0,
        "format_comma(&_1.iter().map(|(x, _)| x).collect())",
        "format_comma(&_1.iter().map(|(_, x)| x).collect())",
    )]
    Phi(Local, Vec<(Local, BasicBlockId)>)
}

#[derive(Debug, Display)]
pub enum Terminator {
    #[display(fmt="goto {}", _0)]
    Goto (BasicBlockId),
    #[display(
        fmt="call {}{}({}){}",
        "if let Some(ret_inner) = ret { format!(\"{} <- \", ret_inner) } else { String::new() }",
        function,
        "format_comma(args)",
        "if let Some(next_inner) = next_block { format!(\", {}\", next_inner) } else { String::new() }",
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
