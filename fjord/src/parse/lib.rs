extern crate fjordtoken;
extern crate fjordlex;
extern crate fjordast;

pub mod defs;
pub mod typeexprs;
pub mod valexprs;

mod util;

#[derive(Clone, Debug)]
pub enum error<l>
{
    lex(l),
    wrongtoken,
}

impl<l> std::fmt::Display for error<l>
    where l: std::fmt::Display
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        match self
        {
            &error::lex(ref l) => l.fmt(f),
            &error::wrongtoken => write!(f, "wrong token"),
        }
    }
}

impl<l> std::error::Error for error<l>
    where l: std::fmt::Debug + std::fmt::Display
{
    fn description(&self) -> &str
    {
        "description() is deprecated; use Display"
    }
}
