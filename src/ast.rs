#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Id(&'a str),
    Lit(Literal<'a>),
    UnOp(Box<Expr<'a>>, UnaryOper),
    BinOp(Box<Expr<'a>>, BinaryOper, Box<Expr<'a>>),
    Ternary {
        cond: Box<Expr<'a>>,
        if_val: Box<Expr<'a>>,
        else_val: Box<Expr<'a>>
    },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOper {
    Plus,
    Minus,
    Not,
    BitNot,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOper {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Equals,
    NotEquals,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AssignOper {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Str(&'a str),
    Dec(&'a str),
    DecDot(&'a str),
    Bool(bool),
    Hex(&'a str),
    Bin(&'a str),
    Oct(&'a str),
}
