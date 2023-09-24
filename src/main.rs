use std::collections::HashMap;

mod eval;
mod term;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let filepath = std::env::args().nth(1).expect("Expected filepath as first argument");
  let file = std::fs::read_to_string(filepath.clone())?;
  let parsed = rinha::parser::parse_or_report(filepath.as_str(), &file)?;

  match eval::eval(&mut HashMap::default(), &mut HashMap::default(), parsed.expression.into()) {
    Ok(term) => println!("Ran successfully! {}", term),
    Err(e) => println!("{:#?}", e),
  };
  Ok(())
}
