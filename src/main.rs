pub mod lex;

use std::{fs, io};

fn main() -> io::Result<()> {
    let code = fs::read_to_string("examples/fizzbuzz.kc")?;
    //let code = fs::read_to_string("examples/hello_world.kc")?;
    
    println!("{:#?}", lex::lex(code));

    Ok(())
}
