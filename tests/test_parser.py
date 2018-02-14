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
        self.stdin_append("extern x();")
        self.stdout_expected([
            "ExternProto(x, [])",
        ])
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_extern_prototype_ok_1_args(self):
        self.stdin_append("extern x(x);")
        self.stdout_expected([
            "ExternProto(x, [\"x\"])",
        ])
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_extern_prototype_ok_2_args(self):
        self.stdin_append("extern x(x y);")
        self.stdout_expected([
            "ExternProto(x, [\"x\", \"y\"])",
        ])
        self.assertKoakListEqual()
        self.assertKoakZeroError()


class UnaryOperatorTest(ParserCustomTestCase):

    def test_basic_neg(self):
        self.stdin_append("-1;")
        self.stdout_expected("TopLevelExpr(Unary(Sub, Number(1.0)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_neg_chained(self):
        self.stdin_append("--1;")
        self.stdout_expected("TopLevelExpr(Unary(Sub, Unary(Sub, Number(1.0))))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_pos(self):
        self.stdin_append("+1;")
        self.stdout_expected("TopLevelExpr(Unary(Add, Number(1.0)))")
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

    def test_function_missing_semicolon(self):
        self.stdin_append("def fib(x) if x < 3 then 1 else fib(x - 1) + fib(x - 2)")
        self.assertKoakLastErrorContain("Missing semi-colon")


class BinOperatorTest(ParserCustomTestCase):
    """
    Test basic linear expression
    """

    def test_basic_1(self):
        self.stdin_append("1 + 2;")
        self.stdout_expected("TopLevelExpr(Binary(Add, Number(1.0), Number(2.0)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_div(self):
        self.stdin_append("1 / 2;")
        self.stdout_expected("TopLevelExpr(Binary(Div, Number(1.0), Number(2.0)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_mul(self):
        self.stdin_append("1 * 2;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Number(1.0), Number(2.0)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_basic_mod(self):
        self.stdin_append("1 % 2;")
        self.stdout_expected("TopLevelExpr(Binary(Rem, Number(1.0), Number(2.0)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_neg(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, Number(1.0), Number(2.0)), Number(3.0)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_add_div(self):
        self.stdin_append("1 + 2 / 3;")
        self.stdout_expected("TopLevelExpr(Binary(Add, Number(1.0), Binary(Div, Number(2.0), Number(3.0))))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_basic_modulo_mul(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, Number(1.0), Number(2.0)), Number(3.0)))")
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_priority_modulo_mul(self):
        self.stdin_append("1 % 2 * 3;")
        self.stdout_expected("TopLevelExpr(Binary(Mul, Binary(Rem, Number(1.0), Number(2.0)), Number(3.0)))")
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
        self.stdin_append("def fib(x) if x < 3 then 1 else fib(x - 1) + fib(x - 2) ;")
        self.stdout_expected("FuncDef(fib, [\"x\"], "
                             "Condition(Binary(Less, Variable(\"x\"), Number(3.0)), "
                             "Number(1.0), "  # return 1
                             "Binary(Add, "  # compute big
                             "Call(\"fib\", [Binary(Sub, Variable(\"x\"), Number(1.0))]), "
                             "Call(\"fib\", [Binary(Sub, Variable(\"x\"), Number(2.0))])"
                             ")))")
        self.assertKoakZeroError()
        self.assertKoakListEqual()


if __name__ == "__main__":
    main()
