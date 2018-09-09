extern crate fjordtoken;
extern crate fjordlex;
extern crate fjordast;
extern crate fjordparse;

use std::error::Error;
use std::io;
use std::io::Read;

fn main()
{
    mainsafe().unwrap();
}

fn mainsafe() -> Result<(), Box<Error>>
{
    let mut source = String::new();
    io::stdin().read_to_string(&mut source)?;
    let mut lexer = fjordlex::source::sourcelexer::new(&source);
    let ast = fjordparse::defs::parsedef(&mut lexer)?;
    println!("{:?}", ast);
    Ok(())
}
