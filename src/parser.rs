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

    // TODO: more expression tests
}
