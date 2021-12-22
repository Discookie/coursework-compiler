use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(language, "/parser/language.rs");
pub use language::ASTParser;

pub mod typecheck;

#[cfg(test)]
mod tests;

