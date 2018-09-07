extern crate fjordtoken;
extern crate fjordlex;
extern crate fjordast;

use fjordlex::lexer;

pub mod defs;
pub mod typeexprs;
pub mod valexprs;

mod util;

#[derive(Clone, Debug)]
pub enum error<l>
    where l: lexer
{
    lex(l::error),
    wrongtoken,
}
