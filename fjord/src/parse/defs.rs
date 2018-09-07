use error;
use fjordast::def;
use fjordast::defbody;
use fjordast::positioned;
use fjordlex::lexer;
use fjordtoken::payload;
use typeexprs::parsetypeexpr;
use util::expect;
use util::expectidentifier;
use valexprs::parsevalexpr;

pub fn parsedef<l>(l: &mut l) -> Result<positioned<def>, error<l>>
    where l: lexer
{
    // Parse the def keyword and the name of the definition.
    let line = expect(l, payload::kwdef)?;
    let (name, _) = expectidentifier(l)?;

    // Parse the signature of the definition.
    expect(l, payload::kwsig)?;
    let sig = parsetypeexpr(l)?;

    // Parse the body of the definition.
    expect(l, payload::kwval)?;
    let body = defbody::val(parsevalexpr(l)?);

    // Parse the end keyword.
    expect(l, payload::kwend)?;

    // Construct the definition AST node.
    let def = def{name, sig, body};
    Ok(positioned{node: def, line})
}
