from unittest import main

from custom_test_case import CustomTestCase


class ParserCustomTestCase(CustomTestCase):

    def init(self):
        super().init()
        self.set_list_args(["-t", "-p"])


class ParserTest(ParserCustomTestCase):
    """
    ParserTest implementation
    """

    def test_extern_prototype_ok_empty_args(self):
        self.stdin_append("extern x() -> int;")
        self.stdout_expected([
            "FunctionDef(x () -> i32)",
        ])
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_extern_prototype_ok_1_args(self):
        self.stdin_append("extern x(x: double) -> int;")
        self.stdout_expected([
            "FunctionDef(x (x: double) -> i32)",
        ])
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_extern_prototype_ok_2_args(self):
        self.stdin_append("extern x(x: bool, y: int) -> double;")
        self.stdout_expected([
            "FunctionDef(x (x: i1, y: i32) -> double)",
        ])
        self.assertKoakListEqual()
        self.assertKoakZeroError()


class UnaryOperatorTest(ParserCustomTestCase):

    def test_basic_neg(self):
        self.stdin_append("-1;")
        self.stdout_expected("TopLevelExpr(Unary(Sub, IntegerLiteral(1)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_neg_chained(self):
        self.stdin_append("--1;")
        self.stdout_expected("TopLevelExpr(Unary(Sub, Unary(Sub, IntegerLiteral(1))))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_pos(self):
        self.stdin_append("+1;")
        self.stdout_expected("TopLevelExpr(Unary(Add, IntegerLiteral(1)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_complex_pos_neg(self):
        self.stdin_append("1.0+-+-+-+-+-+1.0;")
        self.stdout_expected("TopLevelExpr("
                             "Binary(Add, DoubleLiteral(1.0), "
                             "Unary(Sub, "
                             "Unary(Add, "
                             "Unary(Sub, "
                             "Unary(Add, "
                             "Unary(Sub, "
                             "Unary(Add, "
                             "Unary(Sub, "
                             "Unary(Add, "
                             "Unary(Sub, "
                             "Unary(Add, "
                             "DoubleLiteral(1.0)"
                             "))))))))))"
                             ")"
                             ")")
        self.assertKoakListEqual()
        self.assertKoakZeroError()


class DelimiterTest(ParserCustomTestCase):
    """
    Test basic linear expression
    """

    def test_missing_delimiter(self):
        self.stdin_append("1 + 2")
        self.assertKoakLastErrorContain("Missing semi-colon at the end of a top-level expression")

    def test_delimiter_at_end(self):
        self.stdin_append("1 + 2;")
        self.assertKoakZeroError()

    def test_delimiter_at_start(self):
        self.stdin_append(";1 + 2")
        self.assertKoakNeedError()

    def test_delimiter_at_start_and_end(self):
        self.stdin_append(";1 + 2;")
        self.assertKoakZeroError()

    def test_delimiter_only(self):
        self.stdin_append(";")
        self.assertKoakZeroError()

    def test_delimiter_multiple_empty(self):
        self.stdin_append(";;;")
        self.assertKoakZeroError()

    def test_function_missing_semicolon_without_typing(self):
        self.stdin_append("def fib(x) if x < 3 then 1 else fib(x - 1) + fib(x - 2)")
        self.assertKoakLastErrorContain("Argument type is expected")

    def test_function_missing_semicolon(self):
        self.stdin_append("def fib(x: double) -> double if x < 3 then 1 else fib(x - 1) + fib(x - 2)")
        self.assertKoakLastErrorContain("Missing semi-colon")

    def test_delimiter_multiple_expr_and_empty(self):
        self.stdin_append(";;1+2;;;;;;")
        self.stdout_expected("TopLevelExpr(Binary(Add, IntegerLiteral(1), IntegerLiteral(2)))")
        self.assertKoakZeroError()


class BinOperatorTest(ParserCustomTestCase):
    """
    Test basic linear expression
    """

    def test_basic_1(self):
        self.stdin_append("1 + 2;")
        self.stdout_expected("TopLevelExpr(Binary(Add, IntegerLiteral(1), IntegerLiteral(2)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_div(self):
        self.stdin_append("1 / 2;")
        self.stdout_expected("TopLevelExpr(Binary(Div, IntegerLiteral(1), IntegerLiteral(2)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_mul(self):
        self.stdin_append("1 * 2;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, IntegerLiteral(1), IntegerLiteral(2)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_mod(self):
        self.stdin_append("1 % 2;")
        self.stdout_expected("TopLevelExpr(Binary(Rem, IntegerLiteral(1), IntegerLiteral(2)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_neg(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, IntegerLiteral(1), IntegerLiteral(2)), IntegerLiteral(3)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_add_div(self):
        self.stdin_append("1 + 2 / 3;")
        self.stdout_expected("TopLevelExpr(Binary(Add, IntegerLiteral(1), Binary(Div, IntegerLiteral(2), IntegerLiteral(3))))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_modulo_mul(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, IntegerLiteral(1), IntegerLiteral(2)), IntegerLiteral(3)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_modulo_mul(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, IntegerLiteral(1), IntegerLiteral(2)), IntegerLiteral(3)))")
        self.assertKoakListEqual()


class CommentTest(ParserCustomTestCase):
    """
    CommentTest usage
    """

    def test_one_comment(self):
        self.stdin_append("# comment")
        self.assertKoakZeroAll()

    def test_one_comment_2(self):
        self.stdin_append("#comment")
        self.assertKoakZeroAll()

    def test_multiple_ht(self):
        self.stdin_append("### comment")
        self.assertKoakZeroAll()

    def test_comment_in_expression(self):
        self.stdin_append("1 + 2; #comment")
        self.assertKoakZeroError()

    def test_comment_catch_delimiter(self):
        self.stdin_append("1 + 2 # comment ;")
        self.assertKoakLastErrorContain("Missing semi-colon")

    def test_multiline_comment(self):
        self.stdin_append([
            "# One comment"
            "# One comment"
            "# One comment"
            "# One comment"
            "# One comment"
        ])
        self.assertKoakZeroError()


class DefinitionTest(ParserCustomTestCase):

    def test_def_fib(self):
        self.stdin_append("def fib(x: double) -> double if x < 3 then 1 else fib(x - 1) + fib(x - 2) ;")
        self.stdout_expected("FuncDef(fib, [\"x\"], "
                             "Condition(Binary(Less, Variable(\"x\"), Number(3.0)), "
                             "DoubleLiteral(1.0), "  # return 1
                             "Binary(Add, "  # compute big
                             "Call(\"fib\", [Binary(Sub, Variable(\"x\"), DoubleLiteral(1.0))]), "
                             "Call(\"fib\", [Binary(Sub, Variable(\"x\"), Number(2.0))])"
                             ")))")
        self.assertKoakZeroError()
        self.assertKoakListEqual()


if __name__ == "__main__":
    main()
