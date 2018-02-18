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
        self.stdout_expected("TopLevelExpr(Unary(Sub, IntegerLitteral(1)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_neg_chained(self):
        self.stdin_append("--1;")
        self.stdout_expected("TopLevelExpr(Unary(Sub, Unary(Sub, IntegerLitteral(1))))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_pos(self):
        self.stdin_append("+1;")
        self.stdout_expected("TopLevelExpr(Unary(Add, IntegerLitteral(1)))")
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

    def test_delimiter_multiple_expr_and_empty(self):
        self.stdin_append(";;1+2;;;;;;")
        self.stdout_expected("TopLevelExpr(Binary(Add, IntegerLitteral(1), IntegerLitteral(2)))")
        self.assertKoakZeroError()


class BinOperatorTest(ParserCustomTestCase):
    """
    Test basic linear expression
    """

    def test_basic_1(self):
        self.stdin_append("1 + 2;")
        self.stdout_expected("TopLevelExpr(Binary(Add, IntegerLitteral(1), IntegerLitteral(2)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_div(self):
        self.stdin_append("1 / 2;")
        self.stdout_expected("TopLevelExpr(Binary(Div, IntegerLitteral(1), IntegerLitteral(2)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_mul(self):
        self.stdin_append("1 * 2;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, IntegerLitteral(1), IntegerLitteral(2)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_mod(self):
        self.stdin_append("1 % 2;")
        self.stdout_expected("TopLevelExpr(Binary(Rem, IntegerLitteral(1), IntegerLitteral(2)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_neg(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, IntegerLitteral(1), IntegerLitteral(2)), IntegerLitteral(3)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_add_div(self):
        self.stdin_append("1 + 2 / 3;")
        self.stdout_expected("TopLevelExpr(Binary(Add, IntegerLitteral(1), Binary(Div, IntegerLitteral(2), IntegerLitteral(3))))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_modulo_mul(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, IntegerLitteral(1), IntegerLitteral(2)), IntegerLitteral(3)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_modulo_mul(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, IntegerLitteral(1), IntegerLitteral(2)), IntegerLitteral(3)))")
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


if __name__ == "__main__":
    main()
