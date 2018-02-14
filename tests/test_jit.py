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
            "def square(x) x * x",
            "square(5)"
        ])
        self.assertKoakLastOutEqual("=> 25\n")

    def test_square_return_bug_1(self):
        self.stdin_append([
            "def square(x) x * x",
            "square(5)",
            "x = 1",
            "square(5)"
        ])
        self.assertKoakLastOutEqual("=> 25\n")

    def test_fib_multiple(self):
        self.stdin_append([
            "def fib(x) if x < 3 then 1 else fib(x - 1) + fib(x - 2) ;"
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


class UnaryOperatorTest(JITCustomTestCase):

    def test_neg_to_pos(self):
        self.stdin_append([
            "----1;"
        ])
        self.assertKoakLastOutEqual("=> 1\n")


if __name__ == "__main__":
    TextTestRunner.resultclass = ColorTextTestResult
    main(module=None, testRunner=TextTestRunner())
