use lalrpop_util::{lalrpop_mod, ParseError};
use crate::{
    ast::Expr,
    lexer::{Lexer, Token},
};

lalrpop_mod!(grammar);

type Error<'a> = ParseError<usize, Token<'a>, ()>;

pub fn parse_expr<'a>(src: &'a str) -> Result<Expr<'a>, Error<'a>> {
    let lexer = Lexer::new(src);
    let expr = grammar::ExprParser::new()
        .parse(src, lexer);
    expr
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::*,
    };

    #[test]
    fn expr_id() {
        let src = "x";
        let expected = Expr::Id("x");
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn expr_num() {
        let src = "0";
        let expected = Expr::Lit(Literal::Dec("0"));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn expr_strlit() {
        let src = "'foobar'";
        let expected = Expr::Lit(Literal::Str("'foobar'"));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn single_bin_expr() {
        let src = "x + 2";
        let expected = Expr::BinOp(
            Box::new(Expr::Id("x")),
            BinaryOper::Add,
            Box::new(Expr::Lit(Literal::Dec("2"))),
        );
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn single_unary_expr() {
        let src = "+a";
        let expected = Expr::UnOp(Box::new(Expr::Id("a")), UnaryOper::Plus);
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn left_associative() {
        let src = "a + b + c + 1";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::BinOp(
                            Box::new(Expr::Id("a")),
                            BinaryOper::Add,
                            Box::new(Expr::Id("b")))),
                    BinaryOper::Add,
                    Box::new(Expr::Id("c")))),
            BinaryOper::Add,
            Box::new(Expr::Lit(Literal::Dec("1"))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn mult_add_precedence() {
        let src = "a + b * c";
        let expected = Expr::BinOp(
            Box::new(Expr::Id("a")),
            BinaryOper::Add,
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("b")),
                    BinaryOper::Mul,
                    Box::new(Expr::Id("c")))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn and_equals_precedence() {
        let src = "x == y && x != z";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("x")),
                    BinaryOper::Equals,
                    Box::new(Expr::Id("y")))),
            BinaryOper::And,
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("x")),
                    BinaryOper::NotEquals,
                    Box::new(Expr::Id("z")))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn or_less_precedence() {
        let src = "x > 0 || y < 0";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("x")),
                    BinaryOper::Greater,
                    Box::new(Expr::Lit(Literal::Dec("0"))))),
            BinaryOper::Or,
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("y")),
                    BinaryOper::Less,
                    Box::new(Expr::Lit(Literal::Dec("0"))))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn and_or_precedence() {
        let src = "a or b and c";
        let expected = Expr::BinOp(
            Box::new(Expr::Id("a")),
            BinaryOper::Or,
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("b")),
                    BinaryOper::And,
                    Box::new(Expr::Id("c")))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn bitwise_precedence() {
        let src = "a ^ b & c | d";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("a")),
                    BinaryOper::BitXor,
                    Box::new(Expr::BinOp(
                            Box::new(Expr::Id("b")),
                            BinaryOper::BitAnd,
                            Box::new(Expr::Id("c")))))),
            BinaryOper::BitOr,
            Box::new(Expr::Id("d")));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn bitshift_bitwise_precedence() {
        let src = "x << 2 & 0xc";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("x")),
                    BinaryOper::ShiftLeft,
                    Box::new(Expr::Lit(Literal::Dec("2"))))),
            BinaryOper::BitAnd,
            Box::new(Expr::Lit(Literal::Hex("0xc"))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn bitshift_greater_precedence() {
        let src = "x >> y > 64";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("x")),
                    BinaryOper::ShiftRight,
                    Box::new(Expr::Id("y")))),
            BinaryOper::Greater,
            Box::new(Expr::Lit(Literal::Dec("64"))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn bitwise_equals_precedence() {
        // unlike C, this should be (x & MASK) == MASK
        let src = "x & MASK == MASK";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("x")),
                    BinaryOper::BitAnd,
                    Box::new(Expr::Id("MASK")))),
            BinaryOper::Equals,
            Box::new(Expr::Id("MASK")));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn paren_precedence_override() {
        let src = "(a - b) * 1.5";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("a")),
                    BinaryOper::Sub,
                    Box::new(Expr::Id("b")))),
            BinaryOper::Mul,
            Box::new(Expr::Lit(Literal::DecDot("1.5"))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn paren_precedence_override2() {
        let src = "(x + 1) % 4";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("x")),
                    BinaryOper::Add,
                    Box::new(Expr::Lit(Literal::Dec("1"))))),
            BinaryOper::Rem,
            Box::new(Expr::Lit(Literal::Dec("4"))));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn unnecessary_paren_precedence() {
        let src = "(x / y) + z";
        let expected = Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("x")),
                    BinaryOper::Div,
                    Box::new(Expr::Id("y")))),
            BinaryOper::Add,
            Box::new(Expr::Id("z")));
        assert_eq!(expected, parse_expr(src).unwrap());
    }
}
