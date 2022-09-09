pub enum Expr<'a> {
    Id(&'a str),
    Lit(Literal<'a>),
}

pub enum Literal<'a> {
    Str(&'a str),
    Dec(&'a str),
    DecDot(&'a str),
    Bool(bool),
    Hex(&'a str),
    Bin(&'a str),
    Oct(&'a str),
}
