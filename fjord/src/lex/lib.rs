extern crate fjordtoken;

use fjordtoken::token;

pub mod source;

pub trait lexer
{
    type error;
    fn read(&mut self) -> Result<token, Self::error>;
    fn peek(&self) -> Result<token, Self::error>;
}
