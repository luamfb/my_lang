use lalrpop_util::{lalrpop_mod, ParseError};
use crate::{
    ast::{Expr, Stmt},
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

pub fn parse_stmt<'a>(src: &'a str) -> Result<Stmt<'a>, Error<'a>> {
    let lexer = Lexer::new(src);
    let stmt = grammar::StmtParser::new()
        .parse(src, lexer);
    stmt
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::*,
    };

    // ============================== EXPRESSIONS ==============================

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

    #[test]
    fn ternary_expr() {
        let src = "x if x >= 0 else -x";
        let expected = Expr::Ternary {
            cond: Box::new(Expr::BinOp(
                          Box::new(Expr::Id("x")),
                          BinaryOper::GreaterEq,
                          Box::new(Expr::Lit(Literal::Dec("0"))))),
            if_val: Box::new(Expr::Id("x")),
            else_val: Box::new(Expr::UnOp(
                    Box::new(Expr::Id("x")),
                    UnaryOper::Minus)),
        };
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn simple_call_no_args() {
        let src = "foo()";
        let expected = Expr::Call(FnCall::new("foo", vec![]));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn simple_call_one_arg() {
        let src = "print(s)";
        let expected = Expr::Call(FnCall::new("print", vec![Expr::Id("s")]));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn simple_call_multi_args() {
        let src = "add_values(12, 35, -7)";
        let expected = Expr::Call(FnCall::new("add_values", vec![
                                              Expr::Lit(Literal::Dec("12")),
                                              Expr::Lit(Literal::Dec("35")),
                                              Expr::Lit(Literal::Dec("-7"))]));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn call_expr_in_args() {
        let src = "fooBar(x + 1, y % 3, z / 2.0)";

        let arg1 = Expr::BinOp(
            Box::new(Expr::Id("x")),
            BinaryOper::Add,
            Box::new(Expr::Lit(Literal::Dec("1"))));
        let arg2 = Expr::BinOp(
            Box::new(Expr::Id("y")),
            BinaryOper::Rem,
            Box::new(Expr::Lit(Literal::Dec("3"))));
        let arg3 = Expr::BinOp(
            Box::new(Expr::Id("z")),
            BinaryOper::Div,
            Box::new(Expr::Lit(Literal::DecDot("2.0"))));

        let expected = Expr::Call(FnCall::new("fooBar", vec![arg1, arg2, arg3]));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn call_inside_expr() {
        let src = "foo(1, 2) * 3";
        let call = FnCall::new("foo", vec![
                               Expr::Lit(Literal::Dec("1")),
                               Expr::Lit(Literal::Dec("2"))]);
        let expected = Expr::BinOp(
            Box::new(Expr::Call(call)),
            BinaryOper::Mul,
            Box::new(Expr::Lit(Literal::Dec("3"))));

        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn call_with_expr_arg_inside_expr() {
        let src = "add_one(x >> 1) < y";
        let arg = Expr::BinOp(
            Box::new(Expr::Id("x")),
            BinaryOper::ShiftRight,
            Box::new(Expr::Lit(Literal::Dec("1"))));
        let expected = Expr::BinOp(
            Box::new(Expr::Call(FnCall::new("add_one", vec![arg]))),
            BinaryOper::Less,
            Box::new(Expr::Id("y")));

        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn call_with_ternary_arg() {
        let src = "print('positive' if x >= 0 else 'negative')";
        let arg = Expr::Ternary {
            cond: Box::new(Expr::BinOp(
                          Box::new(Expr::Id("x")),
                          BinaryOper::GreaterEq,
                          Box::new(Expr::Lit(Literal::Dec("0"))))),
            if_val: Box::new(Expr::Lit(Literal::Str("'positive'"))),
            else_val: Box::new(Expr::Lit(Literal::Str("'negative'"))),
        };
        let expected = Expr::Call(FnCall::new("print", vec![arg]));
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    #[test]
    fn call_inside_ternary() {
        let src = "halve(n) if n % 2 == 0 else thricePlus1(n)";
        let cond = Box::new(Expr::BinOp(
            Box::new(Expr::BinOp(
                    Box::new(Expr::Id("n")),
                    BinaryOper::Rem,
                    Box::new(Expr::Lit(Literal::Dec("2"))))),
            BinaryOper::Equals,
            Box::new(Expr::Lit(Literal::Dec("0")))));

        let if_val = Box::new(Expr::Call(FnCall::new("halve",
                                                     vec![Expr::Id("n")])));

        let else_val = Box::new(Expr::Call(FnCall::new("thricePlus1",
                                                       vec![Expr::Id("n")])));

        let expected = Expr::Ternary { cond, if_val, else_val };
        assert_eq!(expected, parse_expr(src).unwrap());
    }

    // ============================== STATEMENTS ==============================

    #[test]
    fn call_stmt() {
        let src = "print(s);";
        let expected = Stmt::Call(FnCall::new("print", vec![Expr::Id("s")]));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn assign_stmt() {
        let src = "a = 4;";
        let expected = Stmt::AssignLike(AssignLike::Assign(
                vec!["a"],
                Expr::Lit(Literal::Dec("4"))));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn incr_stmt() {
        let src = "x++;";
        let expected = Stmt::AssignLike(AssignLike::Incr("x"));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn decr_stmt() {
        let src = "i--;";
        let expected = Stmt::AssignLike(AssignLike::Decr("i"));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn add_assign_stmt() {
        let src = "foo += 5;";
        let expected = Stmt::AssignLike(AssignLike::Compound(
                "foo",
                CompoundOper::AddAssign,
                Expr::Lit(Literal::Dec("5"))));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn mul_assign_stmt() {
        let src = "x *= 2;";
        let expected = Stmt::AssignLike(AssignLike::Compound(
                "x",
                CompoundOper::MulAssign,
                Expr::Lit(Literal::Dec("2"))));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn div_assign_stmt() {
        let src = "n /= 7;";
        let expected = Stmt::AssignLike(AssignLike::Compound(
                "n",
                CompoundOper::DivAssign,
                Expr::Lit(Literal::Dec("7"))));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn rem_assign_stmt() {
        let src = "i %= n;";
        let expected = Stmt::AssignLike(AssignLike::Compound(
                "i",
                CompoundOper::RemAssign,
                Expr::Id("n")));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn multi_assign() {
        let src = "a = b = c = 0;";
        let expected = Stmt::AssignLike(AssignLike::Assign(
                vec!["a", "b", "c"],
                Expr::Lit(Literal::Dec("0"))));
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn if_stmt() {
        let src = "if foo {\nbar();\n}";
        let call = Stmt::Call(FnCall::new("bar", vec![]));
        let expected = Stmt::Branch(Expr::Id("foo"), vec![call], vec![]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn if_else_stmt() {
        let src = "if foo {\nf1();\n} else {\nf2();\n}";
        let call1 = Stmt::Call(FnCall::new("f1", vec![]));
        let call2 = Stmt::Call(FnCall::new("f2", vec![]));
        let expected = Stmt::Branch(Expr::Id("foo"), vec![call1], vec![call2]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn for_in_stmt() {
        let src = "for elem in elem_list {\nfoo(elem);\n}";
        let call = Stmt::Call(FnCall::new("foo", vec![Expr::Id("elem")]));
        let expected = Stmt::ForEach("elem", Expr::Id("elem_list"), vec![call]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn c_like_for_stmt() {
        let src = "for i = 0; i < 10; i++ {\nprint(i);\n}";
        let call = Stmt::Call(FnCall::new("print", vec![Expr::Id("i")]));
        let start = AssignLike::Assign(
            vec!["i"],
            Expr::Lit(Literal::Dec("0")));
        let step = Expr::BinOp(
            Box::new(Expr::Id("i")),
            BinaryOper::Less,
            Box::new(Expr::Lit(Literal::Dec("10"))));
        let stop = AssignLike::Incr("i");
        let expected = Stmt::CLikeFor(start, step, stop, vec![call]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn while_stmt() {
        let src = "while cond {\nfoo();\n}";
        let call = Stmt::Call(FnCall::new("foo", vec![]));
        let cond = Expr::Id("cond");
        let expected = Stmt::Loop(LoopKind::While, cond, vec![call]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn until_stmt() {
        let src = "until i > 5 {\ni++;\n}";
        let cond = Expr::BinOp(
            Box::new(Expr::Id("i")),
            BinaryOper::Greater,
            Box::new(Expr::Lit(Literal::Dec("5"))));
        let incr_stmt = Stmt::AssignLike(AssignLike::Incr("i"));
        let expected = Stmt::Loop(LoopKind::Until, cond, vec![incr_stmt]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn do_while_stmt() {
        let src = "do {\ni++;\n} while i < MAX;";
        let cond = Expr::BinOp(
            Box::new(Expr::Id("i")),
            BinaryOper::Less,
            Box::new(Expr::Id("MAX")));
        let incr_stmt = Stmt::AssignLike(AssignLike::Incr("i"));
        let expected = Stmt::Loop(LoopKind::DoWhile, cond, vec![incr_stmt]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn do_until_stmt() {
        let src = "do {\nn--;\n} until n < 0;";
        let cond = Expr::BinOp(
            Box::new(Expr::Id("n")),
            BinaryOper::Less,
            Box::new(Expr::Lit(Literal::Dec("0"))));
        let decr_stmt = Stmt::AssignLike(AssignLike::Decr("n"));
        let expected = Stmt::Loop(LoopKind::DoUntil, cond, vec![decr_stmt]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn ret_stmt() {
        let src = "ret x + 3;";
        let retval = Expr::BinOp(
            Box::new(Expr::Id("x")),
            BinaryOper::Add,
            Box::new(Expr::Lit(Literal::Dec("3"))));
        let expected = Stmt::Ret(retval);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn single_var_decl() {
        let src = "int x;";
        let expected = Stmt::VarDecl(BasicType::Int, vec![("x", None)]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn single_var_decl_with_value() {
        let src = "double x = 0.0;";
        let expected = Stmt::VarDecl(
            BasicType::Double,
            vec![("x", Some(Expr::Lit(Literal::DecDot("0.0"))))]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn multi_var_decl() {
        let src = "uint a, b, c;";
        let expected = Stmt::VarDecl(
            BasicType::Uint,
            vec![("a", None), ("b", None), ("c", None)]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn multi_var_decl_all_values() {
        let src = "int x = 1, y = 0;";
        let assign1 = ("x", Some(Expr::Lit(Literal::Dec("1"))));
        let assign2 = ("y", Some(Expr::Lit(Literal::Dec("0"))));
        let expected = Stmt::VarDecl(BasicType::Int, vec![assign1, assign2]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn multi_var_decl_value_left() {
        let src = "int x = 2, y;";
        let expected = Stmt::VarDecl(
            BasicType::Int,
            vec![("x", Some(Expr::Lit(Literal::Dec("2")))), ("y", None)]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }

    #[test]
    fn multi_var_decl_value_right() {
        let src = "int x, y = -1;";
        let expected = Stmt::VarDecl(
            BasicType::Int,
            vec![("x", None), ("y", Some(Expr::Lit(Literal::Dec("-1"))))]);
        assert_eq!(expected, parse_stmt(src).unwrap());
    }
}
