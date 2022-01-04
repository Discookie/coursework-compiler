use derive_more::Display;

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}", "format_fns(contents)")]
pub struct AST {
    pub contents: Vec<Function>
}

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "fn {}({}){} {{{}}}",
    name,
    "format_comma(args)",
    "if let Some(ret_inner) = ret { format!(\" -> {}\", ret_inner) } else { String::new() }",
    "format_block(body)"
)]

pub struct Function {
    pub name: String,
    pub args: Vec<TypedVar>,
    pub ret: Option<Typename>,
    pub body: Vec<Statement>
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

impl Function {
    pub fn descriptor(&self) -> FnDescriptor {
        let Function {name, args, ..} = self.clone();
        let args = args.into_iter().map(|x| x.typename).collect();

        FnDescriptor { name, args }
    }
}

#[derive(Clone, Debug, Display)]
pub enum Statement {
    #[display(
        fmt = "let {} = {};",
        "if let Some(ty) = typename { format!(\"{}: {}\", name, ty) } else { name.clone() } ",
        value
    )] Define {
        name: String,
        typename: Option<Typename>,
        value: Expr
    },
    #[display(fmt = "{} {} {};", name, op, value)]
    Assign {
        name: String,
        op: AssignOp,
        value: Expr
    },
    #[display(fmt = "{}({});", name, "format_comma(args)")]
    FnCall {
        name: String,
        args: Vec<Expr>
    },
    #[display(fmt = "if {} {{{}}}{}", cond, "format_block(body)", "format_else(else_body)")]
    If {
        cond: Expr,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>
    },
    #[display(fmt = "while {} {{{}}}", cond, "format_block(body)")]
    While {
        cond: Expr,
        body: Vec<Statement>
    },
    #[display(fmt = "for {} in {}..{} {{{}}}", name, from, to, "format_block(body)")]
    For {
        name: String,
        from: Expr,
        to: Expr,
        body: Vec<Statement>
    },
    #[display(fmt = "for {} in {}..={} {{{}}}", name, from, to, "format_block(body)")]
    ForEq {
        name: String,
        from: Expr,
        to: Expr,
        body: Vec<Statement>
    },
    #[display(fmt = "return{};", "if let Some(expr) = expr { format!(\" {}\", expr) } else { String::new() }" )]
    Return {
        expr: Option<Expr>,
    },
    #[display(fmt = "{{{}}}", "format_block(body)")]
    Block {
        body: Vec<Statement>
    }
}

#[derive(Clone, Debug, Display)]
pub enum Expr {
    #[display(fmt = "{}", name)] Var { name: String },
    #[display(fmt = "{}", value)] Const { value: Value },
    #[display(fmt = "{}({})", name, "format_comma(args)")]
    FnCall {
        name: String,
        args: Vec<Expr>
    },
    #[display(fmt = "{}{}", op, right)] UnaryOp {
        op: UnaryOp,
        right: Box<Expr>
    },
    #[display(fmt = "{} {} {}", left, op, right)] BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>
    },
    #[display(fmt = "({})", expr)] Paren {
        expr: Box<Expr>
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum AssignOp {
    #[display(fmt = "=")] Equal,
    #[display(fmt = "+=")] Add,
    #[display(fmt = "-=")] Sub,
    #[display(fmt = "*=")] Mul,
    #[display(fmt = "/=")] Div,
    #[display(fmt = "%=")] Mod,
    #[display(fmt = "&=")] BinAnd,
    #[display(fmt = "|=")] BinOr,
    #[display(fmt = "^=")] Xor,
    #[display(fmt = ">>=")] Rsh,
    #[display(fmt = "<<=")] Lsh
}

#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum UnaryOp {
    #[display(fmt = "+")] Pos,
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

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq)]
pub enum Value {
    #[display(fmt = "{}", _0)] Bool(bool),
    #[display(fmt = "{}", _0)] Int(isize)
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}: {}", name, typename)]
pub struct TypedVar {
    pub name: String,
    pub typename: Typename
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
pub enum Typename {
    #[display(fmt = "int")] Int,
    #[display(fmt = "bool")] Bool
}

impl From<Value> for Typename {
    fn from(value: Value) -> Self {
        match value {
            Value::Bool(_) => Typename::Bool,
            Value::Int(_) => Typename::Int
        }
    }
}

fn format_block<T: std::fmt::Display>(block: &Vec<T>) -> String {
    block.iter()
        .fold(String::new(), |a, b| format!("{}\n{}", a, b))
        .replace("\n", "\n    ")
        + "\n"
}

fn format_fns<T: std::fmt::Display>(functions: &Vec<T>) -> String {
    functions.iter()
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join("\n\n")
        + "\n"
}

fn format_comma<T: std::fmt::Display>(comma: &Vec<T>) -> String {
    comma.iter()
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

fn format_else(else_body: &Option<Vec<Statement>>) -> String {
    if let Some(vec) = else_body {
        match vec.as_slice() {
            [other_if] if matches!(other_if, Statement::If {..}) => 
                format!(" else {}", other_if),
            _ => 
                format!(" else {}", format_block(vec))
        }
    } else {
        "".to_string()
    }
}
