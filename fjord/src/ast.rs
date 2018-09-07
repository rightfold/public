use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct positioned<a>
{
    pub node: a,
    pub line: u64,
}

#[derive(Clone, Debug)]
pub struct def
{
    pub name: Rc<str>,
    pub sig: positioned<typeexpr>,
    pub body: defbody,
}

#[derive(Clone, Debug)]
pub enum defbody
{
    val(positioned<valexpr>),
}

#[derive(Clone, Debug)]
pub enum valexpr
{
    var(Rc<str>),
    abs(Rc<str>, Box<positioned<valexpr>>),
    app(Box<positioned<valexpr>>, Box<positioned<valexpr>>),
}

#[derive(Clone, Debug)]
pub enum typeexpr
{
    var(Rc<str>),
    app(Box<positioned<typeexpr>>, Box<positioned<typeexpr>>),
}
