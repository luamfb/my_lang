use logos::Logos;

pub struct Lexer<'a> {
    lex: logos::SpannedIter<'a, Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            lex: Token::lexer(src).spanned()
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    // LALRPOP expects `Result<(Loc, Tok, Loc), Error>`
    type Item = Result<(usize, Token<'a>, usize), ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let (tok, span) = self.lex.next()?;
        let retval = (span.start, tok, span.end);
        Some(Ok(retval))
    }
}

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'a> {
    #[error]
    #[regex(r"[ \t\n\r\f]+",    logos::skip)]
    #[regex(r"//.*\n",          logos::skip)]
    Invalid,

    #[regex(r"_*[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice())]
    Id(&'a str),

    // TODO backslash sequences
    #[regex(r#""[^"]*""#, |lex| lex.slice())]
    #[regex(r#"'[^']*'"#, |lex| lex.slice())]
    StrLit(&'a str),

    // keywords
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("until")]
    Until,
    #[token("do")]
    Do,
    #[token("in")]
    In,
    #[token("ret")]
    Ret,
    #[token("int")]
    Int,
    #[token("uint")]
    Uint,
    #[token("float")]
    Float,
    #[token("double")]
    Double,
    #[token("byte")]
    Byte,
    #[token("bool")]
    Bool,
    #[token("true")]
    True,
    #[token("false")]
    False,

    // operators
    #[token("++")]
    Incr,
    #[token("--")]
    Decr,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mult,
    #[token("/")]
    Div,
    #[token("%")]
    Rem,
    #[token("==")]
    Equals,
    #[token("!=")]
    NotEquals,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEq,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("&&")]
    #[token("and")]
    And,
    #[token("||")]
    #[token("or")]
    Or,
    #[token("!")]
    #[token("not")]
    Not,
    #[token("&")]
    BitAnd,
    #[token("|")]
    BitOr,
    #[token("^")]
    BitXor,
    #[token("~")]
    BitNot,
    #[token("=")]
    Assign,
    // TODO compound assignment operators e.g. `+=`

    // symbols
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(r"\")]
    Backslash,
    #[token("?")]
    Question,
    #[token("@")]
    At,
    #[token("#")]
    Hash,
    #[token("$")]
    Dollar,
}

// TODO tests
