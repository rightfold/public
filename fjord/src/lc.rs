use std::rc::Rc;

pub enum valexpr
{
    var(Rc<str>),
    abs(Rc<str>, Box<valexpr>),
    app(Box<valexpr>, Box<valexpr>),
}
