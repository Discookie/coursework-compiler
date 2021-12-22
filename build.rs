use std::path::Path;

fn main() {
    let examples = Path::new("./examples");
    let language = Path::new("./src/parser/language.lalrpop");
    println!("cargo:rerun-if-changed={}", examples.display());
    println!("cargo:rerun-if-changed={}", language.display());
    lalrpop::process_root().unwrap();
}