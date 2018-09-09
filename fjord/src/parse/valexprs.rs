use error;
use fjordast::positioned;
use fjordast::valexpr;
use fjordlex::lexer;
use fjordtoken::payload;
use util::expect;
use util::expectidentifier;
use util::peek;
use util::positionedlike;
use util::read;

pub fn parsevalexpr<l>(l: &mut l) -> Result<positioned<valexpr>, error<l::error>>
    where l: lexer
{
    parselevel2(l)
}

pub fn parselevel2<l>(l: &mut l) -> Result<positioned<valexpr>, error<l::error>>
    where l: lexer
{
    let mut left = parselevel1(l)?;
    loop
    {
        match (peek(l)?.payload)
        {
            payload::identifier(_) => (),
            payload::kwfun => (),
            payload::puparenleft => (),
            _ => break,
        }

        let right = parselevel1(l)?;

        left = positionedlike(left, |left|
            valexpr::app(
                Box::new(left),
                Box::new(right),
            ),
        );
    }
    Ok(left)
}

pub fn parselevel1<l>(l: &mut l) -> Result<positioned<valexpr>, error<l::error>>
    where l: lexer
{
    // TODO: Report the possible token types in the default match case.

    let token = read(l)?;
    match (token.payload)
    {
        // Parse a variable expression.
        payload::identifier(name) => {
            let node = valexpr::var(name);
            Ok(positioned{node, line: token.line})
        },

        // Parse an abstraction expression.
        payload::kwfun => {
            let (param, _) = expectidentifier(l)?;
            expect(l, payload::puthinarrow)?;
            let body = parsevalexpr(l)?;
            let node = valexpr::abs(param, Box::new(body));
            Ok(positioned{node, line: token.line})
        },

        // Parse a parenthesized expression.
        payload::puparenleft => {
            let inner = parsevalexpr(l)?;
            expect(l, payload::puparenright)?;
            Ok(inner)
        },

        _ => Err(error::wrongtoken),
    }
}
