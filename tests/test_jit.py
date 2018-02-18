from unittest import main, TextTestRunner

from unittestcolor import ColorTextTestResult
from custom_test_case import CustomTestCase


class JITCustomTestCase(CustomTestCase):

    def init(self):
        super().init()
        self.set_list_args(["-t"])
        self.input_type_piped()


class DefinitionTest(JITCustomTestCase):

    def test_square_definition_25(self):
        self.stdin_append([
            "def square(x: int) -> int x * x;",
            "square(11);"
        ])
        self.assertKoakLastOutEqual("=> {}\n".format(11 * 11))

    def test_square_definition_25_by_25(self):
        self.stdin_append([
            "def square(x: int) -> int x * x;",
            "square(5) * square(5);"
        ])
        self.assertKoakLastOutEqual("=> {}\n".format(25 * 25))

    def test_square_return_bug_1(self):
        self.stdin_append([
            "def square(x: int) -> int x * x;",
            "square(5);",
            "x = 1;",
            "square(5);"
        ])
        self.assertKoakLastOutEqual("=> 25\n")

    def test_fib_multiple(self):
        self.stdin_append([
            "def fib(x: int) -> int if x < 3 then 1 else fib(x - 1) + fib(x - 2) ;"
            "fib(2);fib(3);fib(4);fib(5);fib(6);"
        ])
        self.stdout_expected([
            "=> 1",
            "=> 2",
            "=> 3",
            "=> 5",
            "=> 8",
        ])
        self.assertKoakListEqual()

    def test_definition_in_body(self):
        self.stdin_append("def f(x:int) -> int def t(y: int) -> int x * x;")
        self.assertKoakLastErrorContain("An expression was expected")

    def test_definition_without_typing(self):
        self.stdin_append("def f(x) -> int x * x;")
        self.assertKoakLastErrorContain("Argument type is expected")

    def test_definition_without_delimiter(self):
        self.stdin_append("def f(x:int) -> int x * x")
        self.assertKoakLastErrorContain("Missing semi-colon after an")

    def test_definition_without_return_typing(self):
        self.stdin_append("def f(x:int) x * x")
        self.assertKoakLastErrorContain("Return type is expected")


class RedefinitionDefinitionTests(JITCustomTestCase):

    def test_redefinition_of_f_only(self):
        self.stdin_append([
            "def f(x: int) -> int x;"
            "def f(x: int) -> int x;"
        ])
        self.assertKoakLastErrorContain("Redefinition of function \"f\"")

    def test_redefinition_of_f(self):
        self.stdin_append([
            "def f(x: int) -> int x;"
            "5;",
            "def f(x: int) -> int x;"
        ])
        self.assertKoakNeedError()


class UnaryOperatorTest(JITCustomTestCase):

    def test_neg_to_pos(self):
        self.stdin_append([
            "----1;"
        ])
        self.assertKoakLastOutEqual("=> 1\n")


if __name__ == "__main__":
    TextTestRunner.resultclass = ColorTextTestResult
    main(module=None, testRunner=TextTestRunner())
