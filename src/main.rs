use std::{collections::HashMap, thread};

mod eval;
mod term;

const STACK_SIZE: usize = 1024 * 1024 * 1024;

fn main() -> Result<(), String> {
  fn go() -> Result<(), String> {
    let filepath = std::env::args().nth(1).expect("Expected filepath as first argument");
    let file = std::fs::read_to_string(filepath.clone()).map_err(|e| e.to_string())?;
    let parsed = rinha::parser::parse_or_report(filepath.as_str(), &file).map_err(|e| e.to_string())?;

    match eval::eval(&mut HashMap::default(), &mut HashMap::default(), parsed.expression.into()) {
      Ok(_term) => {}
      Err(e) => println!("{:#?}", e),
    };
    Ok(())
  }

  // Spawn thread with explicit stack size
  let child = thread::Builder::new().stack_size(STACK_SIZE).spawn(go).unwrap();

  // Wait for thread to join
  child.join().unwrap()
}
