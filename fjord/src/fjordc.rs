extern crate fjordtoken;
extern crate fjordlex;
extern crate fjordast;
extern crate fjordparse;
extern crate fjordllvm;

use std::error::Error;
use std::ffi::CStr;
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

    let llvmctx = fjordllvm::context::new();
    let llvmmod = fjordllvm::module::new(&llvmctx, <&CStr>::default());

    let fjheap  = llvmctx.namedstructtype(CStr::from_bytes_with_nul(b"fjheap\0" ).unwrap()).pointertype();
    let fjvalue = llvmctx.namedstructtype(CStr::from_bytes_with_nul(b"fjvalue\0").unwrap()).pointertype();

    let square = llvmmod.addfunction(
        CStr::from_bytes_with_nul(b"square\0").unwrap(),
        fjordllvm::tipe::functiontype(fjvalue, &[fjheap]),
    );

    llvmmod.dump();

    Ok(())
}
