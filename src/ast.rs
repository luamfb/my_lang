pub type SrcFile<'a> = Vec<Toplevel<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub enum Toplevel<'a> {
    Impl(FnImpl<'a>),
    Decl(FnPrototype<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnImpl<'a> {
    prototype: FnPrototype<'a>,
    body: Vec<Stmt<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnPrototype<'a> {
    fn_name: &'a str,
    args: Vec<(BasicType, &'a str)>,
    ret_type: Option<BasicType>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'a> {
    VarDecl(BasicType, Vec<(&'a str, Option<Expr<'a>>)>),
    Ret(Expr<'a>),
    Call(FnCall<'a>),
    AssignLike(AssignLike<'a>),
    Branch(Expr<'a>, Vec<Stmt<'a>>, Vec<Stmt<'a>>),
    /// Loops involving while or until
    Loop(LoopKind, Expr<'a>, Vec<Stmt<'a>>),
    ForEach(&'a str, Expr<'a>, Vec<Stmt<'a>>),
    CLikeFor(AssignLike<'a>, Expr<'a>, AssignLike<'a>, Vec<Stmt<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignLike<'a> {
    Assign(Vec<&'a str>, Expr<'a>),
    Compound(&'a str, CompoundOper, Expr<'a>),
    Incr(&'a str),
    Decr(&'a str),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LoopKind {
    While,
    Until,
    DoWhile,
    DoUntil,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BasicType {
    Int,
    Uint,
    Bool,
    Byte,
    Float,
    Double,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Id(&'a str),
    Lit(Literal<'a>),
    Call(FnCall<'a>),
    UnOp(Box<Expr<'a>>, UnaryOper),
    BinOp(Box<Expr<'a>>, BinaryOper, Box<Expr<'a>>),
    Ternary {
        cond: Box<Expr<'a>>,
        if_val: Box<Expr<'a>>,
        else_val: Box<Expr<'a>>
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnCall<'a> {
    name: &'a str,
    args: Vec<Expr<'a>>,
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
pub enum CompoundOper {
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

impl<'a> FnImpl<'a> {
    pub fn new(prototype: FnPrototype<'a>, body: Vec<Stmt<'a>>) -> Self {
        FnImpl {
            prototype,
            body,
        }
    }
}

impl<'a> FnPrototype<'a> {
    pub fn new(fn_name: &'a str,
               args: Vec<(BasicType, &'a str)>,
               ret_type: Option<BasicType>) -> Self {
        FnPrototype {
            fn_name,
            args,
            ret_type,
        }
    }
}

impl<'a> FnCall<'a> {
    pub fn new(name: &'a str, args: Vec<Expr<'a>>) -> Self {
        FnCall {
            name,
            args,
        }
    }
}
