use error;
use fjordast::positioned;
use fjordlex::lexer;
use fjordtoken::payload;
use fjordtoken::token;
use std::rc::Rc;

// Position an AST node like another AST node, avoiding issues with lifetimes.
pub fn positionedlike<f, a, b>(old: positioned<a>, new: f) -> positioned<b>
    where f: FnOnce(positioned<a>) -> b
{
    let line = old.line;
    positioned{node: new(old), line: line}
}

pub fn read<l>(l: &mut l) -> Result<token, error<l::error>>
    where l: lexer
{
    l.read().map_err(error::lex)
}

pub fn peek<l>(l: &l) -> Result<token, error<l::error>>
    where l: lexer
{
    l.peek().map_err(error::lex)
}

pub fn expect<l>(l: &mut l, payload: payload) -> Result<u64, error<l::error>>
    where l: lexer
{
    let token = read(l)?;
    if (token.payload == payload)
    {
        Ok(token.line)
    }
    else
    {
        Err(error::wrongtoken)
    }
}

pub fn expectidentifier<l>(l: &mut l) -> Result<(Rc<str>, u64), error<l::error>>
    where l: lexer
{
    let token = read(l)?;
    match (token.payload)
    {
        payload::identifier(a) => Ok((a, token.line)),
        _ => Err(error::wrongtoken),
    }
}

