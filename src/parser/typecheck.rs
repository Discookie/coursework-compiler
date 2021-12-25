use std::{error::Error, collections::HashMap, fmt::Display};

use super::ast::*;

#[inline]
fn print_option<T: Display>(elem: Option<T>) -> String {
    match elem {
        Some(x) => x.to_string(),
        None => String::new()
    }
}

#[inline]
fn assert_type(expected: Option<Typename>, actual: Option<Typename>, description: &str) -> Result<(), Box<dyn Error>> {
    if expected != actual {
        return Err(format!("{}: expected {}, actual {}", description, print_option(expected), print_option(actual)).into());
    }

    Ok(())
}

// Type requirements

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeRequirement {
    Same,
    /// None means no requirement
    Certain(Option<Typename>, Option<Typename>)
}

pub trait RequiresType {
    fn requires_type(&self) -> TypeRequirement;
}

impl RequiresType for AssignOp {
    fn requires_type(&self) -> TypeRequirement {
        match self {
            AssignOp::Equal => TypeRequirement::Same,
            _ => TypeRequirement::Certain(Some(Typename::Int), Some(Typename::Int))
        }
    }
}

impl RequiresType for UnaryOp {
    fn requires_type(&self) -> TypeRequirement {
        match self {
            UnaryOp::Not => TypeRequirement::Certain(None, None),
            _ => TypeRequirement::Certain(Some(Typename::Int), None)
        }
    }
}

impl RequiresType for BinOp {
    fn requires_type(&self) -> TypeRequirement {
        match self {
            BinOp::Or | BinOp::And => TypeRequirement::Certain(Some(Typename::Bool), Some(Typename::Bool)),
            BinOp::Eq | BinOp::NEq => TypeRequirement::Same,
            _ => TypeRequirement::Certain(Some(Typename::Int), Some(Typename::Int))
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeCreation {
    Same,
    /// None means creating None
    Certain(Option<Typename>)
}

pub trait CreatesType {
    fn creates_type(&self) -> TypeCreation;
}

impl CreatesType for BinOp {
    fn creates_type(&self) -> TypeCreation {
        match self {
            BinOp::Eq | BinOp::NEq |
            BinOp::Lt | BinOp::Gt |
            BinOp::LEq | BinOp::GEq
                => TypeCreation::Certain(Some(Typename::Bool)),
            _ => TypeCreation::Same
        }
    }
}

// Type checking

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Typestate {
    Full(Option<Typename>),
    Partial(Option<Typename>),
    None
}

impl Typestate {
    pub fn require_full(&self, description: &str) -> Result<(), Box<dyn Error>> {
        if !matches!(self, Typestate::Full(_)) {
            return Err(format!("{}: not all code paths return a value", description).into())
        }

        Ok(())
    }

    pub fn require_defined(&self, description: &str) -> Result<(), Box<dyn Error>> {
        if !matches!(self, Typestate::Full(Some(_))) {
            return Err(format!("{}: not all code paths return a value", description).into())
        }

        Ok(())
    }

    pub fn assert_type(&self, expected: Option<Typename>, description: &str) -> Result<(), Box<dyn Error>> {
        match self {
            Typestate::Partial(actual) | Typestate::Full(actual) => 
                assert_type(expected, *actual, description),
            _ => Ok(())
        }
    }

    pub fn or(&self, other: Typestate, description: &str) -> Result<Typestate, Box<dyn Error>> {
        match (*self, other) {
            (Typestate::Full(mine), Typestate::Full(other)) |
            (Typestate::Full(mine), Typestate::Partial(other)) |
            (Typestate::Partial(mine), Typestate::Full(other)) |
            (Typestate::Partial(mine), Typestate::Partial(other))
                if mine != other => Err(format!(
                    "{}: left {}, right {}",
                    description,
                    print_option(mine.as_ref()),
                    print_option(other.as_ref())
                ).into()),

            (Typestate::Full(ty), _) |
            (_, Typestate::Full(ty))
                => Ok(Typestate::Full(ty)),

            (Typestate::Partial(ty), _) |
            (_, Typestate::Partial(ty))
                => Ok(Typestate::Partial(ty)),

            (Typestate::None, Typestate::None) => Ok(Typestate::None)
        }
    }

    pub fn and(&self, other: Typestate, description: &str) -> Result<Typestate, Box<dyn Error>> {
        match (*self, other) {
            (Typestate::Full(mine), Typestate::Full(other)) |
            (Typestate::Full(mine), Typestate::Partial(other)) |
            (Typestate::Partial(mine), Typestate::Full(other)) |
            (Typestate::Partial(mine), Typestate::Partial(other))
                if mine != other => Err(format!(
                    "{}: left {}, right {}",
                    description,
                    print_option(mine.as_ref()),
                    print_option(other.as_ref())
                ).into()),

            (Typestate::Full(ty), Typestate::Full(_))
                => Ok(Typestate::Full(ty)),

            (Typestate::Full(ty), _) |
            (Typestate::Partial(ty), _) |
            (_, Typestate::Full(ty)) |
            (_, Typestate::Partial(ty))
                => Ok(Typestate::Partial(ty)),

            (Typestate::None, Typestate::None) => Ok(Typestate::None)
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeResolverCtxt {
    variables: HashMap<String, Typename>,
    functions: HashMap<FnDescriptor, Option<Typename>>,

    change_stack: Vec<HashMap<String, Option<Typename>>>
}

impl TypeResolverCtxt {
    pub fn new() -> TypeResolverCtxt {
        TypeResolverCtxt {
            variables: HashMap::new(),
            functions: HashMap::new(),
            change_stack: Vec::new()
        }
    }

    #[cfg(test)]
    pub fn stack_height(&self) -> usize {
        self.change_stack.len()
    }

    pub fn push_stack(&mut self) {
        self.change_stack.push(HashMap::new());
    }

    pub fn pop_stack(&mut self) {
        let top = self.change_stack.pop().expect("Typecheck: Stack misaligned (pop)");

        for (name, ty) in top {
            match ty {
                Some(ty) => self.variables.insert(name, ty),
                None => self.variables.remove(&name)
            };
        }
    }

    #[inline]
    fn top_stack(&self) -> &HashMap<String, Option<Typename>> {
        self.change_stack.last().expect("Typecheck: Stack misalignment (top)")
    }

    #[inline]
    fn top_stack_mut(&mut self) -> &mut HashMap<String, Option<Typename>> {
        self.change_stack.last_mut().expect("Typecheck: Stack misalignment (top)")
    }

    pub fn add_variable(&mut self, name: &String, ty: Typename) -> Result<(), Box<dyn Error>> {
        let old_value = self.variables.insert(name.clone(), ty);

        if self.top_stack().contains_key(name) {
            return Err(format!("Variable already declared: {}", name).into());
        }

        self.top_stack_mut().insert(name.clone(), old_value);

        Ok(())
    }

    pub fn get_variable(&self, name: &String) -> Result<Typename, Box<dyn Error>> {
        self.variables.get(name).cloned().ok_or( format!("Undefined variable: {}", name).into())
    }

    pub fn add_function(&mut self, func: FnDescriptor, ret: Option<Typename>) -> Result<(), Box<dyn Error>> {
        let err = format!("Redefined function {}", func);
        
        if self.functions.insert(func, ret).is_some() {
            return Err(err.into());
        }

        Ok(())
    }

    pub fn check_function(&self, func: &FnDescriptor) -> Result<(), Box<dyn Error>> {
        if !self.functions.contains_key(func) {
            return Err(format!("Undefined function {}", func).into());
        }
        
        Ok(())
    }

    pub fn find_function(&self, func: &FnDescriptor) -> Result<Option<Typename>, Box<dyn Error>> {
        if let Some(ret) = self.functions.get(func) {
            Ok(*ret)
        } else {
            Err(format!("Undefined function {}", func).into())
        }
    }
}

pub trait TypeResolverVisitor {
    fn resolve_type(&self, tcx: &mut TypeResolverCtxt) -> Result<Typestate, Box<dyn Error>>;
}

impl TypeResolverVisitor for AST {
    fn resolve_type(&self, tcx: &mut TypeResolverCtxt) -> Result<Typestate, Box<dyn Error>> {
        let default_funcs = vec![
            (FnDescriptor { name: "write".to_string(), args: vec![Typename::Bool] }, None),
            (FnDescriptor { name: "write".to_string(), args: vec![Typename::Int] }, None),
            (FnDescriptor { name: "read_bool".to_string(), args: vec![] }, Some(Typename::Bool)),
            (FnDescriptor { name: "read_int".to_string(), args: vec![] }, Some(Typename::Int)),
        ];

        for (func, ret) in default_funcs {
            tcx.add_function(func, ret)?;
        }

        for func in self.contents.iter() {
            tcx.add_function(func.descriptor(), func.ret)?;
        }

        for func in self.contents.iter() {
            func.resolve_type(tcx)?;
        }

        let ret_ty = tcx.find_function(&FnDescriptor { name: "main".to_string(), args: Vec::new() })?;

        if matches!(ret_ty, Some(Typename::Bool)) {
            return Err("Main function has invalid return type".into());
        }

        Ok(Typestate::None)
    }
}

impl TypeResolverVisitor for Function {
    fn resolve_type(&self, tcx: &mut TypeResolverCtxt) -> Result<Typestate, Box<dyn Error>> {
        tcx.push_stack();

        for TypedVar { name, typename } in self.args.iter() {
            tcx.add_variable(name, *typename)?;
        }

        let body_ty = self.body.resolve_type(tcx)?;

        if self.ret.is_some() {
            body_ty.require_full(&format!("Returning from function {}", self.descriptor()))?;
        }

        body_ty.assert_type(self.ret, &format!("Return type mismatch in function {}", self.descriptor()))?;

        tcx.pop_stack();
        Ok(Typestate::None)
    }
}

impl TypeResolverVisitor for Vec<Statement> {
    fn resolve_type(&self, tcx: &mut TypeResolverCtxt) -> Result<Typestate, Box<dyn Error>> {
        tcx.push_stack();

        let mut return_ty = Typestate::None;

        for stmt in self.iter() {
            let ty = stmt.resolve_type(tcx)?;
            return_ty = return_ty.or(ty, "Block has incompatible returns")?;
        }
        
        tcx.pop_stack();

        Ok(return_ty)
    }
}

impl TypeResolverVisitor for Statement {
    fn resolve_type(&self, tcx: &mut TypeResolverCtxt) -> Result<Typestate, Box<dyn Error>> {
        match self {
            Statement::Define { name, typename, value } => {
                let ty = value.resolve_type(tcx)?;

                ty.require_defined(&format!("Defining {}", name))?;

                if let Some(expected_ty) = typename {
                    ty.assert_type(Some(*expected_ty), &format!("Type mismatch defining {}", name))?;
                }
                
                match ty {
                    Typestate::Full(Some(ty)) => tcx.add_variable(name, ty)?,
                    _ => return Err("Internal error".into())
                }

                Ok(Typestate::None)
            },

            Statement::Assign { name, op, value } => {
                let var = tcx.get_variable(name)?;
                let ty = value.resolve_type(tcx)?;

                let err = format!("Operator type mismatch: {} {} ...", name, op);

                match op.requires_type() {
                    TypeRequirement::Same => ty.assert_type(Some(var), &err)?,
                    TypeRequirement::Certain(left, right) => {
                        if left.is_some() {
                            assert_type(Some(var), left, &err)?;
                        }
                        if right.is_some() {
                            ty.assert_type(right, &err)?;
                        }
                    },
                }

                Ok(Typestate::None)
            },

            Statement::FnCall { name, args } => {
                let resolved_args: Vec<Result<Typestate, Box<dyn Error>>> = args.iter().map(|arg| arg.resolve_type(tcx)).collect();
                let mut ensured_args: Vec<Typename> = Vec::with_capacity(resolved_args.len());

                for arg in resolved_args {
                    if let Typestate::Full(Some(ty)) = arg? {
                        ensured_args.push(ty);
                    } else {
                        return Err("Invalid fn argument type".into());
                    }
                }

                let descriptor = FnDescriptor { name: name.clone(), args: ensured_args };

                tcx.check_function(&descriptor)?;

                Ok(Typestate::None)
            },

            Statement::If { cond, body, else_body } => {
                cond.resolve_type(tcx)?.assert_type(Some(Typename::Bool), "Invalid if condition")?;

                let mut ty = body.resolve_type(tcx)?;
                if let Some(else_body) = else_body {
                    let else_body_ty = else_body.resolve_type(tcx)?;
                    ty = ty.and(else_body_ty, "If-else has incompatible returns")?;
                }

                Ok(ty)
            },

            Statement::While { cond, body } => {
                cond.resolve_type(tcx)?.assert_type(Some(Typename::Bool), "Invalid while condition")?;

                let body_ty = body.resolve_type(tcx)?;

                Ok(body_ty.and(Typestate::None, "Internal error")?)
            }

            Statement::For { name, from, to, body } |
            Statement::ForEq { name, from, to, body } => {
                tcx.push_stack();

                
                from.resolve_type(tcx)?.assert_type(Some(Typename::Int), "Invalid from number in for")?;
                to.resolve_type(tcx)?.assert_type(Some(Typename::Int), "Invalid to number in for")?;

                tcx.add_variable(name, Typename::Int)?;
                
                let body_ty = body.resolve_type(tcx)?;
                
                tcx.pop_stack();
                Ok(body_ty.and(Typestate::None, "Internal error")?)
            },
            Statement::Return { expr } => {
                Ok(expr.resolve_type(tcx)?)
            },
            Statement::Block { body } => {
                Ok(body.resolve_type(tcx)?)
            },
        }
    }
}

impl TypeResolverVisitor for Expr {
    fn resolve_type(&self, tcx: &mut TypeResolverCtxt) -> Result<Typestate, Box<dyn Error>> {
        match self {
            Expr::Var { name } => {
                let ty = tcx.get_variable(name)?;
                Ok(Typestate::Full(Some(ty)))
            },
            Expr::Const { value } => Ok(Typestate::Full(Some((*value).into()))),
            Expr::FnCall { name, args } => {
                let resolved_args: Vec<Result<Typestate, Box<dyn Error>>> = args.iter().map(|arg| arg.resolve_type(tcx)).collect();
                let mut ensured_args: Vec<Typename> = Vec::with_capacity(resolved_args.len());

                for arg in resolved_args {
                    if let Typestate::Full(Some(ty)) = arg? {
                        ensured_args.push(ty);
                    } else {
                        return Err("Invalid fn argument type".into());
                    }
                }

                let ty = tcx.find_function(&FnDescriptor { name: name.clone(), args: ensured_args })?;

                Ok(Typestate::Full(ty))
            },
            Expr::UnaryOp { op, right } => {
                let ty = right.resolve_type(tcx)?;

                match op.requires_type() {
                    TypeRequirement::Certain(Some(expected), _) => {
                        ty.assert_type(Some(expected), &format!("Operator type mismatch: {}", op))?;
                    }
                    _ => ()
                }

                Ok(ty)
            },
            Expr::BinOp { left, op, right } => {
                let left_ty = left.resolve_type(tcx)?;
                let right_ty = right.resolve_type(tcx)?;

                match op.requires_type() {
                    TypeRequirement::Same => {
                        left_ty.or(right_ty, &format!("Operator type mismatch: {} ...", op))?;
                    },
                    TypeRequirement::Certain(left, right) => {
                        if left.is_some() {
                            left_ty.assert_type( left, &format!("Operator type mismatch: {} ...", op))?;
                        }
                        if right.is_some() {
                            right_ty.assert_type(right, &format!("Operator type mismatch: {} ...", op))?;
                        }
                    },
                }

                match op.creates_type() {
                    TypeCreation::Same => Ok(left_ty),
                    TypeCreation::Certain(ty) => Ok(Typestate::Full(ty))
                }
            },
            Expr::Paren { expr } => expr.resolve_type(tcx),
        }
    }
}
