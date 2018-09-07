use error;
use fjordast::positioned;
use fjordast::typeexpr;
use fjordlex::lexer;
use fjordtoken::payload;
use std::rc::Rc;
use util::expect;
use util::expectidentifier;
use util::peek;
use util::positionedlike;
use util::read;

pub fn parsetypeexpr<l>(l: &mut l) -> Result<positioned<typeexpr>, error<l>>
    where l: lexer
{
    parselevel2(l)
}

pub fn parselevel2<l>(l: &mut l) -> Result<positioned<typeexpr>, error<l>>
    where l: lexer
{
    // Parse the left hand side of a function type arrow, or the type
    // expression itself if no function type arrow follows.
    let domain = parselevel1(l)?;

    // If a function type arrow follows, parse the right hand side and
    // construct the function type.
    if (peek(l)?.payload == payload::puthinarrow)
    {
        // Parse the function type arrow.
        let arrowline = read(l)?.line;

        // Parse the right hand side of the function type arrow.
        let codomain = parselevel2(l)?;

        // Construct the function type expression, applying operator-> to the
        // domain and the codomain.
        let functypeexpr = positionedlike(domain, |domain|
            typeexpr::app(
                Box::new(
                    positionedlike(domain, |domain|
                        typeexpr::app(
                            Box::new(positioned{
                                // TODO: Create a module with built-in names.
                                node: typeexpr::var(Rc::from("->")),
                                line: arrowline,
                            }),
                            Box::new(domain),
                        ),
                    ),
                ),
                Box::new(codomain),
            )
        );
        Ok(functypeexpr)
    }

    // No function type arrow follows, just return the type expression.
    else
    {
        Ok(domain)
    }
}

pub fn parselevel1<l>(l: &mut l) -> Result<positioned<typeexpr>, error<l>>
    where l: lexer
{
    // TODO: Match on the token and report the possible token types in the
    // TODO: default match case.

    let token = peek(l)?;

    // Parse a parenthesized type expression.
    if (token.payload == payload::puparenleft)
    {
        read(l)?;
        let inner = parsetypeexpr(l)?;
        expect(l, payload::puparenright)?;
        Ok(inner)
    }

    // Parse a variable type expression.
    else
    {
        let (name, line) = expectidentifier(l)?;
        let node = typeexpr::var(name);
        Ok(positioned{node, line})
    }
}
