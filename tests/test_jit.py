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
        self.assertKoakLastErrorContain("Missing semi-colon at the end of a function definition")

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
        self.assertKoakLastErrorContain("Redefinition of function \"f\"")


class UnaryOperatorTest(JITCustomTestCase):

    def test_neg_to_pos_int(self):
        self.stdin_append([
            "----1;"
        ])
        self.assertKoakLastOutEqual("=> 1\n")

    def test_neg_to_pos_double(self):
        self.stdin_append([
            "----1.5;"
        ])
        self.assertKoakLastOutEqual("=> 1.5\n")

    def test_neg_to_pos_bool(self):
        self.stdin_append([
            "----true;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_neg_to_pos_char(self):
        self.stdin_append([
            "----'a';"
        ])
        self.assertKoakLastOutEqual("=> 'a'\n")

class AutoCastTests(JITCustomTestCase):
    def test_bool_casts(self):
        self.stdin_append([
            'import "unit_test";',

            # Bool to bool
            "to_bool(true);",
            "to_bool(false);",

            # Char to bool
            "to_bool('a');",
            "to_bool('b');",
            "to_bool('\\0');",

            # Int to bool
            "to_bool(-1);",
            "to_bool(0);",
            "to_bool(1);",

            # Float to bool
            "to_bool(-1.5);",
            "to_bool(-0.001);",
            "to_bool(0.0);",
            "to_bool(0.001);",
            "to_bool(1.0);",
            "to_bool(2.0);",
        ])
        self.stdout_expected([
            "=> true",
            "=> false",

            "=> true",
            "=> true",
            "=> false",

            "=> true",
            "=> false",
            "=> true",

            "=> true",
            "=> true",
            "=> false",
            "=> true",
            "=> true",
            "=> true",
        ])
        self.assertKoakListEqual()

    def test_char_casts(self):
        self.stdin_append([
            'import "unit_test";',

            # Bool to char
            "to_char(false);",
            "to_char(true);",

            # Char to char
            "to_char('a');",
            "to_char('\\t');",
            "to_char('\\0');",

            # Int to char
            "to_char(97);",
            "to_char(98);",
            "to_char(-1);",

            # Double to char
            "to_char(99.5);",
            "to_char(100.99);",
            "to_char(-1/2);",
        ])
        self.stdout_expected([
            "=> '\\u{0}'",
            "=> '\\u{1}'",

            "=> 'a'",
            "=> '\\t'",
            "=> '\\u{0}'",

            "=> 'a'",
            "=> 'b'",
            "=> '\\u{ff}'",

            "=> 'c'",
            "=> 'd'",
            "=> '\\u{0}'",
        ])
        self.assertKoakListEqual()

    def test_int_casts(self):
        self.stdin_append([
            'import "unit_test";',

            # Bool to Int
            "to_int(true);",
            "to_int(false);",

            # Char to Int
            "to_int('a');",
            "to_int('b');",
            "to_int('\\0');",

            # Int to Int
            "to_int(-1);",
            "to_int(0);",
            "to_int(1);",

            # Float to Int
            "to_int(-1.5);",
            "to_int(-0.001);",
            "to_int(0.0);",
            "to_int(0.001);",
            "to_int(1.0);",
            "to_int(2.0);",
        ])
        self.stdout_expected([
            "=> 1",
            "=> 0",

            "=> 97",
            "=> 98",
            "=> 0",

            "=> -1",
            "=> 0",
            "=> 1",

            "=> -1",
            "=> 0",
            "=> 0",
            "=> 0",
            "=> 1",
            "=> 2",
        ])
        self.assertKoakListEqual()

    def test_double_casts(self):
        self.stdin_append([
            'import "unit_test";',

            # Bool to Double
            "to_double(true);",
            "to_double(false);",

            # Char to Double
            "to_double('a');",
            "to_double('b');",
            "to_double('\\0');",

            # Int to Double
            "to_double(-1);",
            "to_double(0);",
            "to_double(1);",

            # Float to Double
            "to_double(-1.5);",
            "to_double(1.0/3.0);",
        ])
        self.stdout_expected([
            "=> 1",
            "=> 0",

            "=> 97",
            "=> 98",
            "=> 0",

            "=> -1",
            "=> 0",
            "=> 1",

            "=> -1.5",
            "=> 0.3333333333333333",
        ])
        self.assertKoakListEqual()

class VoidTests(JITCustomTestCase):
    def test_void_call(self):
        self.stdin_append("extern putchar(c: char) -> void; def f() -> void putchar('a'); f();")
        self.assertKoakLastOutEqual("a\n")

    def test_void_binop_void(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "putchar('a') + putchar('a');"
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('Invalid binary operator for type "void" and "void"')

    def test_void_binop_char(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "'a' + putchar('a');"
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('Invalid binary operator for type "i8" and "void"')

    def test_void_binop_bool(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "true + putchar('a');"
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('Invalid binary operator for type "i1" and "void"')

    def test_void_binop_int(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "1 + putchar('a');"
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('Invalid binary operator for type "i32" and "void"')

    def test_void_binop_double(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "1.5 + putchar('a');"
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('Invalid binary operator for type "double" and "void"')

    def test_void_param(self):
        self.stdin_append([
            "extern putchar(c: void) -> void;",
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('The "void" type can only be used as a return type of a function')

    def test_invalid_void_cond(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "if 1 < 2 then putchar('a') else 2;",
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('If bodies\'s type doesn\'t match. Got "void" on one side, and "i32" on the other side')

    #def test_void_cond(self):
    #    self.stdin_append([
    #        "extern putchar(c: char) -> void;",
    #        "if 1 < 2 then putchar('a') else putchar('a');",
    #    ])
    #    self.assertKoakLastOutEqual("a\n")

    def test_void_binop_cond(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "1 + if 1 < 2 then putchar('a') else putchar('a');",
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('Invalid binary operator for type "i32" and "void"')

class ForLoopTests(JITCustomTestCase):
    def test_with_step(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "for x = 0, x < 10, 2 in putchar('0' + x);",
        ])
        self.assertKoakLastOutEqual("02468\n")

    def test_without_step(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "for x = 0, x < 10 in putchar('0' + x);",
        ])
        self.assertKoakLastOutEqual("0123456789\n")

    def test_returned_value(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def test(y: int) -> int for x = 0, x < y in 1;",
            "test(10);",
        ])
        self.assertKoakLastOutEqual("=> 10\n")

    def test_returned_value_2(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "for x = 1, false in 1;",
        ])
        self.assertKoakLastOutEqual("=> 1\n")

    def test_backward_for(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "for x = 9, x > -1, -1 in putchar('0' + x);",
        ])
        self.assertKoakLastOutEqual("9876543210\n")

    def test_for_scopes_1(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def func(x: int) -> void for i = x, i < x + 10 in putchar('a' + i);",
            "5 + x;",
        ])
        self.assertKoakLastErrorContain("Undefined variable \"x\"")

    def test_for_scopes_2(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def func(x: int) -> void for i = x, i < x + 10 in putchar('a' + i);",
            "func(5);",
        ])
        self.assertKoakLastOutEqual("fghijklmno\n")

    def test_for_scopes_3(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def func(x: int) -> void for x = 0, x < 10 in putchar('a' + x);",
            "func(20);",
        ])
        self.assertKoakLastOutEqual("abcdefghij\n")

    def test_for_scopes_4(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def func(x: int) -> int if x > 5 then for x = 0, x < 10 in putchar('0' + x) else x;",
            "func(20);",
            "func(2);",
        ])
        self.stdout_expected([
            "=> 10",
            "=> 2",
            "0123456789",
        ])
        self.assertKoakListEqual()

    def test_for_scopes_5(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def func(x: int) -> int if x > 5 then for y = 0, y < 10 in putchar('0' + y) else y;",
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain("Undefined variable \"y\"")

if __name__ == "__main__":
    TextTestRunner.resultclass = ColorTextTestResult
    main(module=None, testRunner=TextTestRunner())
