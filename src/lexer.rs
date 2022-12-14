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

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token<'a> {
    #[error]
    #[regex(r"[ \t\n\r\f]+",    logos::skip)]
    #[regex(r"//.*\n",          logos::skip)]
    Invalid,

    #[regex(r"_*[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice())]
    Id(&'a str),

    #[token("0",                    |lex| lex.slice())]
    #[regex(r"[+-]?[0-9][0-9_]*",   |lex| lex.slice())]
    DecNum(&'a str),
    #[regex(r"[+-]?[0-9][0-9_]*\.[0-9][0-9_]*", |lex| lex.slice())]
    DecDotNum(&'a str),

    #[regex(r"0x[0-9a-fA-F][0-9a-fA-F_]*",  |lex| lex.slice())]
    HexNum(&'a str),
    #[regex(r"0o[0-7][0-7_]*",              |lex| lex.slice())]
    OctNum(&'a str),
    #[regex(r"0b[01][01_]*",                |lex| lex.slice())]
    BinNum(&'a str),

    #[regex(r#""([^\\"]|\\([\\"'aefnrtv]|x[0-9a-fA-F][0-9a-fA-F]))*""#, |lex| lex.slice())]
    #[regex(r#"'([^\\']|\\([\\"'aefnrtv]|x[0-9a-fA-F][0-9a-fA-F]))*'"#, |lex| lex.slice())]
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
    #[token("<<")]
    ShiftLeft,
    #[token(">>")]
    ShiftRight,
    #[token("=")]
    Assign,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("%=")]
    RemAssign,
    #[token("&=")]
    BitAndAssign,
    #[token("|=")]
    BitOrAssign,
    #[token("^=")]
    BitXorAssign,
    #[token("<<=")]
    ShiftLeftAssign,
    #[token(">>=")]
    ShiftRightAssign,

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let src = "";
        let mut lexer = Lexer::new(src);
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn single_id_one_char() {
        let src = "x";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::Id("x"), 1);
        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn single_id() {
        let src = "foobar";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::Id("foobar"), 6);
        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn single_id_underscores() {
        let src = "__abc_def";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::Id("__abc_def"), 9);
        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn single_id_camel_case() {
        let src = "camelCaseId";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::Id("camelCaseId"), 11);
        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn single_kw() {
        let src = "if";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::If, 2);
        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn expr_add() {
        let src = "a+b";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::Id("a"), 1);
        let span2 = (1, Token::Plus, 2);
        let span3 = (2, Token::Id("b"), 3);

        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), Some(Ok(span2)));
        assert_eq!(lexer.next(), Some(Ok(span3)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn if_clause() {
        let src = "if x == y";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::If, 2);
        let span2 = (3, Token::Id("x"), 4);
        let span3 = (5, Token::Equals, 7);
        let span4 = (8, Token::Id("y"), 9);

        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), Some(Ok(span2)));
        assert_eq!(lexer.next(), Some(Ok(span3)));
        assert_eq!(lexer.next(), Some(Ok(span4)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn var_decl() {
        let src = "int x;";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::Int, 3);
        let span2 = (4, Token::Id("x"), 5);
        let span3 = (5, Token::Semicolon, 6);

        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), Some(Ok(span2)));
        assert_eq!(lexer.next(), Some(Ok(span3)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn decimal_literals() {
        let src = "-1 0 +0.7 123_456 -3.14_16";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::DecNum("-1"), 2);
        let span2 = (3, Token::DecNum("0"), 4);
        let span3 = (5, Token::DecDotNum("+0.7"), 9);
        let span4 = (10, Token::DecNum("123_456"), 17);
        let span5 = (18, Token::DecDotNum("-3.14_16"), 26);

        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), Some(Ok(span2)));
        assert_eq!(lexer.next(), Some(Ok(span3)));
        assert_eq!(lexer.next(), Some(Ok(span4)));
        assert_eq!(lexer.next(), Some(Ok(span5)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn non_decimal_literals() {
        let src = "0x12 0b1101_0010 0xDEAD_BEEF 0o775";
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::HexNum("0x12"), 4);
        let span2 = (5, Token::BinNum("0b1101_0010"), 16);
        let span3 = (17, Token::HexNum("0xDEAD_BEEF"), 28);
        let span4 = (29, Token::OctNum("0o775"), 34);

        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), Some(Ok(span2)));
        assert_eq!(lexer.next(), Some(Ok(span3)));
        assert_eq!(lexer.next(), Some(Ok(span4)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn simple_strlit() {
        let src = r#"'single' "double""#;
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::StrLit("'single'"), 8);
        let span2 = (9, Token::StrLit("\"double\""), 17);
        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), Some(Ok(span2)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn strlit_backslash_seq() {
        let src = r#"'don\'t' "foo\n" '\x61\x5f\x2D'"#;
        let mut lexer = Lexer::new(src);
        let span1 = (0, Token::StrLit("'don\\'t'"), 8);
        let span2 = (9, Token::StrLit("\"foo\\n\""), 16);
        let span3 = (17, Token::StrLit("'\\x61\\x5f\\x2D'"), 31);
        assert_eq!(lexer.next(), Some(Ok(span1)));
        assert_eq!(lexer.next(), Some(Ok(span2)));
        assert_eq!(lexer.next(), Some(Ok(span3)));
        assert_eq!(lexer.next(), None);
    }

    //TODO more tests
}
