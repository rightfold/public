use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct token
{
    pub payload: payload,
    pub line: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum payload
{
    eof,

    identifier(Rc<str>),

    puthinarrow,

    pucolon,
    puparenleft,
    puparenright,

    kwdef,
    kwend,
    kwfun,
    kwsig,
    kwval,
}
