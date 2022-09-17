use lalrpop_util::{lalrpop_mod, ParseError};
use crate::{
    ast::{Expr, Stmt, SrcFile},
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

pub fn parse_src_file<'a>(src: &'a str) -> Result<SrcFile<'a>, Error<'a>> {
    let lexer = Lexer::new(src);
    let src_file = grammar::SrcFileParser::new()
        .parse(src, lexer);
    src_file
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

    // ============================== SOURCE FILE ==============================

    #[test]
    fn simplest_fn_decl() {
        let src = "foo();";
        let prototype = FnPrototype::new("foo", vec![], None);
        let expected = vec![Toplevel::Decl(prototype)];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn single_fn_decl_1arg() {
        let src = "print_num(int n);";
        let prototype = FnPrototype::new("print_num",
                                         vec![(BasicType::Int, "n")],
                                         None);
        let expected = vec![Toplevel::Decl(prototype)];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn single_fn_decl_1arg_with_ret() {
        let src = "twice(int x) int;";
        let prototype = FnPrototype::new("twice",
                                         vec![(BasicType::Int, "x")],
                                         Some(BasicType::Int));
        let expected = vec![Toplevel::Decl(prototype)];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn single_fn_decl_multi_args() {
        let src = "doStuff(int a, double b, byte c);";
        let args = vec![
            (BasicType::Int, "a"),
            (BasicType::Double, "b"),
            (BasicType::Byte, "c"),
        ];
        let prototype = FnPrototype::new("doStuff", args, None);
        let expected = vec![Toplevel::Decl(prototype)];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn single_fn_decl_multi_args_with_ret() {
        let src = "addThree(int x, int y, int z) int;";
        let args = vec![
            (BasicType::Int, "x"),
            (BasicType::Int, "y"),
            (BasicType::Int, "z"),
        ];
        let prototype = FnPrototype::new("addThree", args, Some(BasicType::Int));
        let expected = vec![Toplevel::Decl(prototype)];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn multi_fn_decl() {
        let src = "foo1() byte;\nfoo2(int x, double y);\nfoo3(float a, float b) float;";
        let args_foo2 = vec![(BasicType::Int, "x"), (BasicType::Double, "y")];
        let args_foo3 = vec![(BasicType::Float, "a"), (BasicType::Float, "b")];

        let proto1 = FnPrototype::new("foo1", vec![], Some(BasicType::Byte));
        let proto2 = FnPrototype::new("foo2", args_foo2, None);
        let proto3 = FnPrototype::new("foo3", args_foo3, Some(BasicType::Float));

        let expected = vec![
            Toplevel::Decl(proto1),
            Toplevel::Decl(proto2),
            Toplevel::Decl(proto3),
        ];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn nop_fn_impl() {
        let src = "doNothing() {\n}";
        let proto = FnPrototype::new("doNothing", vec![], None);
        let expected = vec![Toplevel::Impl(FnImpl::new(proto, vec![]))];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn simple_fn_impl() {
        let src = "twice(int x) int {\nret x * 2;\n}";
        let proto = FnPrototype::new(
            "twice",
            vec![(BasicType::Int, "x")],
            Some(BasicType::Int));
        let ret_stmt = Stmt::Ret(Expr::BinOp(
                Box::new(Expr::Id("x")),
                BinaryOper::Mul,
                Box::new(Expr::Lit(Literal::Dec("2")))));
        let body = vec![ret_stmt];
        let expected = vec![Toplevel::Impl(FnImpl::new(proto, body))];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn simple_fn_impl2() {
        let src = "addTwo(int a, int b) int {\nret a + b;\n}";
        let proto = FnPrototype::new(
            "addTwo",
            vec![(BasicType::Int, "a"), (BasicType::Int, "b")],
            Some(BasicType::Int));
        let ret_stmt = Stmt::Ret(Expr::BinOp(
                Box::new(Expr::Id("a")),
                BinaryOper::Add,
                Box::new(Expr::Id("b"))));
        let body = vec![ret_stmt];
        let expected = vec![Toplevel::Impl(FnImpl::new(proto, body))];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn fn_impl_multi_stmt_body() {
        let src = r#"uint_pow(double x, uint n) double {
            double ans = 1.0;
            uint i;
            for i = 0; i < n; i++ {
                ans *= x;
            }
            ret ans;
        }"#;

        let decl1 = Stmt::VarDecl(
            BasicType::Double,
            vec![("ans", Some(Expr::Lit(Literal::DecDot("1.0"))))]);

        let decl2 = Stmt::VarDecl(BasicType::Uint, vec![("i", None)]);

        let compare_expr = Expr::BinOp(
            Box::new(Expr::Id("i")),
            BinaryOper::Less,
            Box::new(Expr::Id("n")));
        let mult_assign_stmt = Stmt::AssignLike(AssignLike::Compound(
                "ans",
                CompoundOper::MulAssign,
                Expr::Id("x")));
        let for_stmt = Stmt::CLikeFor(
            AssignLike::Assign(vec!["i"], Expr::Lit(Literal::Dec("0"))),
            compare_expr,
            AssignLike::Incr("i"),
            vec![mult_assign_stmt]);

        let ret_stmt = Stmt::Ret(Expr::Id("ans"));

        let body = vec![
            decl1,
            decl2,
            for_stmt,
            ret_stmt,
        ];
        let proto = FnPrototype::new(
            "uint_pow",
            vec![(BasicType::Double, "x"), (BasicType::Uint, "n")],
            Some(BasicType::Double));

        let expected = vec![Toplevel::Impl(FnImpl::new(proto, body))];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }

    #[test]
    fn multi_fn_impl() {
        let src = r#"
        addTwo(int x, int y) int {
            ret x + y;
        }
        addThree(int a, int b, int c) int {
            ret a + addTwo(b, c);
        }"#;

        let proto1 = FnPrototype::new(
            "addTwo",
            vec![(BasicType::Int, "x"), (BasicType::Int, "y")],
            Some(BasicType::Int));
        let ret1 = Stmt::Ret(Expr::BinOp(
                Box::new(Expr::Id("x")),
                BinaryOper::Add,
                Box::new(Expr::Id("y"))));
        let fn_impl1 = Toplevel::Impl(FnImpl::new(proto1, vec![ret1]));

        let proto2 = FnPrototype::new(
            "addThree",
            vec![
                (BasicType::Int, "a"),
                (BasicType::Int, "b"),
                (BasicType::Int, "c"),
            ],
            Some(BasicType::Int));
        let ret2 = Stmt::Ret(Expr::BinOp(
                Box::new(Expr::Id("a")),
                BinaryOper::Add,
                Box::new(Expr::Call(FnCall::new(
                        "addTwo",
                        vec![Expr::Id("b"), Expr::Id("c")])))));
        let fn_impl2 = Toplevel::Impl(FnImpl::new(proto2, vec![ret2]));

        let expected = vec![fn_impl1, fn_impl2];
        assert_eq!(expected, parse_src_file(src).unwrap());
    }
}
