mod eval;
mod term;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let filepath = "examples/add.rinha";
  let file = std::fs::read_to_string(filepath)?;
  let parsed = rinha::parser::parse_or_report(filepath, &file)?;

  match eval::eval(&vec![], parsed.expression.into()) {
    Ok(term) => println!("Ran successfully! {}", term),
    Err(e) => println!("{:#?}", e),
  };
  Ok(())
}
