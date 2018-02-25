from unittest import main, TextTestRunner, skip

from custom_test_case import CustomTestCase
from unittestcolor import ColorTextTestResult


class JITCustomTestCase(CustomTestCase):

    def init(self):
        super().init()
        self.set_list_args(["-t"])
        self.input_type_piped()

    def _to_res(self, obj):
        if isinstance(obj, bool):
            return "=> " + ("true" if obj else "false")
        else:
            return "=> " + str(obj)

    def def_putchar(self):
        self.stdin_append("extern putchar(c: char) -> void;")

    def tearDown(self):
        super().tearDown()
        self.assertKoakListErrNotContain("LLLM", False)
        self.assertKoakListOutNotContain("LLLM", False)


class DefinitionTest(JITCustomTestCase):

    def test_square_definition_25(self):
        self.stdin_append([
            "def square(x: int) -> int { x * x }",
            "square(11);"
        ])
        self.assertKoakLastOutEqual("=> {}\n".format(11 * 11))

    def test_square_definition_25_by_25(self):
        self.stdin_append([
            "def square(x: int) -> int { x * x }",
            "square(5) * square(5);"
        ])
        self.assertKoakLastOutEqual("=> {}\n".format(25 * 25))

    def test_square_return_bug_1(self):
        self.stdin_append([
            "def square(x: int) -> int { x * x }",
            "square(5);",
            "x = 1;",
            "square(5);"
        ])
        self.assertKoakLastOutEqual("=> 25\n")

    def test_fib_multiple(self):
        self.stdin_append([
            "def fib(x: int) -> int { if (x < 3) 1 else fib(x - 1) + fib(x - 2) }"
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
        self.stdin_append("def f(x:int) -> int { def t(y: int) -> int { x * x}  } ")
        self.assertKoakLastErrorContain("An expression was expected")

    def test_definition_without_typing(self):
        self.stdin_append("def f(x) -> int x * x;")
        self.assertKoakLastErrorContain("Argument type is expected")

    def test_definition_without_bracket(self):
        self.stdin_append("def f(x:int) -> int { x * x")
        self.assertKoakLastErrorContain("Unterminated block")

    def test_definition_without_return_typing(self):
        self.stdin_append("def f(x:int) x * x")
        self.assertKoakLastErrorContain("Return type is expected")


class RedefinitionDefinitionTests(JITCustomTestCase):

    def test_redefinition_of_f_only(self):
        self.stdin_append([
            "def f(x: int) -> int { x }"
            "def f(x: int) -> int { x }"
        ])
        self.assertKoakLastErrorContain("Redefinition of function \"f\"")

    def test_redefinition_of_f_diff_nb_args(self):
        self.stdin_append([
            "def f(x: int) -> int { x }"
            "extern f(x: int, y: int) -> int;"
        ])
        self.assertKoakLastErrorContain("Function \"f\" redefined with different arguments")

    def test_redefinition_of_f_diff_args_types(self):
        self.stdin_append([
            "def f(x: int) -> int { x }"
            "extern f(x: double) -> int;"
        ])
        self.assertKoakLastErrorContain("Function \"f\" redefined with different arguments")

    def test_redefinition_of_f_diff_ret_type(self):
        self.stdin_append([
            "def f(x: int) -> double { x }"
            "extern f(x: int) -> int;"
        ])
        self.assertKoakLastErrorContain("Function \"f\" redefined with different arguments")

    def test_redefinition_of_f(self):
        self.stdin_append([
            "def f(x: int) -> int { x }"
            "5;",
            "def f(x: int) -> int { x }"
        ])
        self.assertKoakLastErrorContain("Redefinition of function \"f\"")

    def test_redefinition_tricky(self):
        self.stdin_append([
            "extern f(x: int) -> int;"
            "def f(x: int) -> int { x * 3 }"
            "extern f(x: int) -> int;"
            "f(5)"
        ])
        self.assertKoakLastOutEqual("=> 15\n")

    def test_redefinition_tricky2(self):
        self.stdin_append([
            "extern f(x: int) -> int;"
            "def f(x: int) -> int { x * 3 }"
            "extern f(x: int) -> int;"
            "def f(x: int) -> int { x * 4 }"
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

    def test_unary_neg(self):
        self.stdin_append([
            "!true;",
            "!'a';",
            "!1;",
            "!0.1;",
        ])
        self.stdout_expected([
            self._to_res(not True),
            self._to_res(not 'a'),
            self._to_res(not 1),
            self._to_res(not 0.1)
        ])
        self.assertKoakListEqual()

    def test_unary_compl(self):
        self.stdin_append([
            # "~true;",
            # "~'a';",
            "~1;",
        ])

        self.stdout_expected([
            # self._to_res(~True),
            # self._to_res(chr(~ord('a'))),
            self._to_res(~1)
        ])
        self.assertKoakListEqual()


class AutoCastTests(JITCustomTestCase):
    def test_bool_casts(self):
        self.stdin_append([
            'import "../examples/std";',

            # Bool to bool
            "as_bool(true);",
            "as_bool(false);",

            # Char to bool
            "as_bool('a');",
            "as_bool('b');",
            "as_bool('\\0');",

            # Int to bool
            "as_bool(-1);",
            "as_bool(0);",
            "as_bool(1);",

            # Double to bool
            "as_bool(-1.5);",
            "as_bool(-0.001);",
            "as_bool(0.0);",
            "as_bool(0.001);",
            "as_bool(1.0);",
            "as_bool(2.0);",
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
            'import "../examples/std";',

            # Bool to char
            "as_char(false);",
            "as_char(true);",

            # Char to char
            "as_char('a');",
            "as_char('\\t');",
            "as_char('\\0');",

            # Int to char
            "as_char(97);",
            "as_char(98);",
            "as_char(-1);",

            # Double to char
            "as_char(99.5);",
            "as_char(100.99);",
            "as_char(-1/2);",
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
            'import "../examples/std";',

            # Bool to Int
            "as_int(true);",
            "as_int(false);",

            # Char to Int
            "as_int('a');",
            "as_int('b');",
            "as_int('\\0');",

            # Int to Int
            "as_int(-1);",
            "as_int(0);",
            "as_int(1);",

            # Double to Int
            "as_int(-1.5);",
            "as_int(-0.001);",
            "as_int(0.0);",
            "as_int(0.001);",
            "as_int(1.0);",
            "as_int(2.0);",
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
            'import "../examples/std";',

            # Bool to Double
            "as_double(true);",
            "as_double(false);",

            # Char to Double
            "as_double('a');",
            "as_double('b');",
            "as_double('\\0');",

            # Int to Double
            "as_double(-1);",
            "as_double(0);",
            "as_double(1);",

            # Double to Double
            "as_double(-1.5);",
            "as_double(1.0/3.0);",
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
        self.stdin_append("extern putchar(c: char) -> void; def f() -> void { putchar('a') } f();")
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
        self.assertKoakLastErrorContain('Invalid binary operator for type "char" and "void"')

    def test_void_binop_bool(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "true + putchar('a');"
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('Invalid binary operator for type "bool" and "void"')

    def test_void_binop_int(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "1 + putchar('a');"
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('Invalid binary operator for type "int" and "void"')

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
            "if 1 < 2 putchar('a') else 2;",
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain(
            'If bodies\'s type doesn\'t match. Got "void" on one side, and "int" on the other side')

    def test_void_cond(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "if 1 < 2 putchar('a') else putchar('a')",
        ])
        self.assertKoakLastOutEqual("a\n")

    def test_void_binop_cond(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "1 + if 1 < 2 { putchar('a') else putchar('a') }",
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain('An expression was expected')

    def test_void_as_cond(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "if putchar('a') putchar('b') else putchar('c')",
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain("Can't cast type \"void\" to type \"bool\"")


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
            "for x = 0, x < 10 in putchar('0' + x)",
        ])
        self.assertKoakLastOutEqual("0123456789\n")

    def test_returned_value(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def test(y: int) -> int { for x = 0, x < y in 1 }",
        ])
        self.assertKoakLastErrorContain("Can't cast type \"void\" to type \"int\"")

    def test_returned_value_2(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "for x = 1, false in 1",
        ])
        self.assertKoakZeroOut

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
            "def func(x: int) -> void { for i = x, i < x + 10 in putchar('a' + i) }",
            "func(5);",
        ])
        self.assertKoakLastOutEqual("fghijklmno\n")

    def test_for_scopes_3(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def func(x: int) -> void { for x = 0, x < 10 in putchar('a' + x)}",
            "func(20);",
        ])
        self.assertKoakLastOutEqual("abcdefghij\n")

    def test_for_scopes_4(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def func(x: int) -> int { if x > 5 { for x = 0, x < 10 in putchar('0' + x); x * 2 } else x }",
            "func(20);",
            "func(2);",
        ])
        self.stdout_expected([
            "=> 40",
            "=> 2",
            "0123456789",
        ])
        self.assertKoakListEqual()

    def test_for_scopes_5(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "def func(x: int) -> int { if x > 5 { for y = 0, y < 10 in putchar('0' + y); x } else y }",
        ])
        self.assertKoakZeroOut()
        self.assertKoakLastErrorContain("Undefined variable \"y\"")


class CmpOperatorTest(JITCustomTestCase):

    def test_equal_integer_integer_zero(self):
        self.stdin_append([
            "0==0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_equal_integer_integer_false(self):
        self.stdin_append([
            "1==0;"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_equal_double_integer_zero(self):
        self.stdin_append([
            "0.0==0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_equal_integer_double_zero(self):
        self.stdin_append([
            "0==0.0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_equal_double_double_zero(self):
        self.stdin_append([
            "0.0==0.0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_equal_double_double_false(self):
        self.stdin_append([
            "0.1==0.11;"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_diff_integer_integer_zero(self):
        self.stdin_append([
            "0!=0;"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_diff_integer_integer_false(self):
        self.stdin_append([
            "1!=0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_diff_double_integer_zero(self):
        self.stdin_append([
            "0.0!=0;"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_diff_integer_double_zero(self):
        self.stdin_append([
            "0!=0.0;"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_diff_double_double_zero(self):
        self.stdin_append([
            "0.0!=0.0;"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_diff_double_double_false(self):
        self.stdin_append([
            "0.1!=0.11;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_diff_double_empty(self):
        self.stdin_append([
            "0.1!=;"
        ])
        self.assertKoakLastErrorContain("An expression was expected")

    def test_diff_with_parenthesis(self):
        self.stdin_append([
            "(5 + 10) != (10 + 5);"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_diff_equal_without_parenthesis(self):
        self.stdin_append([
            "1 != 2 == 1;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_diff_equal_without_parenthesis_false(self):
        self.stdin_append([
            "1.1 != 2.0 == 1.2;"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_equal_triple_error(self):
        self.stdin_append([
            "1==="
        ])
        self.assertKoakLastErrorContain("An expression was expected")

    def test_diff_triple_error(self):
        self.stdin_append([
            "1!=="
        ])
        self.assertKoakLastErrorContain("An expression was expected")

    def test_leq_integer_integer_zero(self):
        self.stdin_append([
            "0<=0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_leq_integer_integer_false(self):
        self.stdin_append([
            "1<=0;"
        ])
        self.assertKoakLastOutEqual("=> false\n")

    def test_leq_double_integer_zero(self):
        self.stdin_append([
            "0.0<=0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_leq_integer_double_zero(self):
        self.stdin_append([
            "0<=0.0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_leq_double_double_zero(self):
        self.stdin_append([
            "0.0<=0.0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_geq_integer_integer_zero(self):
        self.stdin_append([
            "0<=0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_geq_integer_char_zero(self):
        self.stdin_append([
            "0<='a';"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_geq_integer_integer_true(self):
        self.stdin_append([
            "1>=0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_geq_double_integer_zero(self):
        self.stdin_append([
            "0.0>=0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_geq_integer_double_zero(self):
        self.stdin_append([
            "0>=0.0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_geq_double_double_zero(self):
        self.stdin_append([
            "0.0>=0.0;"
        ])
        self.assertKoakLastOutEqual("=> true\n")


class BlockTests(JITCustomTestCase):
    def test_block_return_val_1(self):
        self.stdin_append([
            'import "../examples/std";',
            "def f() -> int { putc('a') } ;"
        ])
        self.assertKoakLastErrorContain("Can't cast type \"void\" to type \"int\"")

    def test_block_return_val_2(self):
        self.stdin_append([
            'import "../examples/std";',
            "def f() -> int { putc('a'); } ;",
        ])
        self.assertKoakLastErrorContain("Can't cast type \"void\" to type \"int\"")

    def test_block_return_val_3(self):
        self.stdin_append([
            'import "../examples/std";',
            "def f() -> int { putc('a'); 5 } ;",
            "f();",
            "putc('\\n');"
        ])
        self.stdout_expected([
            "=> 5",
            "a",
        ])
        self.assertKoakListEqual()

    def test_block_return_val_4(self):
        self.stdin_append([
            'import "../examples/std";',
            "def f() -> int { putc('a'); 5; } ;",
        ])
        self.assertKoakLastErrorContain("Can't cast type \"void\" to type \"int\"")

    def test_nested_blocks_1(self):
        self.stdin_append([
            'import "../examples/std";',
            "def f(x: int) -> void { if x > 0 for i = 0, i < 10 in putc('0' + i) else for i = 0, i < 10 in putc('9' - i) }",
            "f(5);",
            "f(-1);",
        ])
        self.assertKoakLastOutEqual("01234567899876543210\n")

    def test_nested_blocks_2(self):
        self.stdin_append([
            'import "../examples/std";',
            "def f() -> void { for i = 0, i < 10 in for i = 0, i < 10 in putc('0' + i) }",
            "f();",
        ])
        self.assertKoakLastOutEqual("0123456789" * 10 + "\n")


class ConditionTests(JITCustomTestCase):
    def test_cond(self):
        self.stdin_append([
            "def func(x: int) -> bool { if x > 3 true else false } "
            "func(5);"
            "func(0);"
        ])
        self.stdout_expected([
            "=> true",
            "=> false",
        ])
        self.assertKoakListEqual()

    def test_cond_no_else(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(x: int) -> void { if x > 3 putcln('a') }"
            "func(5);"
            "func(0);"
        ])
        self.stdout_expected([
            "a"
        ])
        self.assertKoakListEqual()

    def test_cond_no_else_as_val_error(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(x: int) -> int { if x > 3 x }"
        ])
        self.assertKoakLastErrorContain("Can't cast type \"void\" to type \"int\"")

    def test_cond_no_else_as_val_ok(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(x: int) -> void { if x > 3 x }"
            "func(5);"
            "func(10);"
        ])
        self.assertKoakZeroOut()

    def test_cond_no_else_as_val_ok2(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(x: int) -> void { if x > 3 x; }"
            "func(5);"
            "func(10);"
        ])
        self.assertKoakZeroOut()

    def test_cond_void_ret(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(x: int) -> void { if x < 10 { putchar('0' + x); putchar('\\n'); } else { putchar('?'); putchar('\\n') } }"
            "func(5);"
            "func(15);"
        ])
        self.stdout_expected([
            "5",
            "?",
        ])
        self.assertKoakListEqual()

    def test_cond_else_if(self):
        self.stdin_append([
            "def test(x: double) -> char { if x < 0 'N' else if x > 0 'P' else '0' }"
            "test(-1.5);"
            "test(0);"
            "test(1.75);"
        ])
        self.stdout_expected([
            "=> 'N'",
            "=> '0'",
            "=> 'P'",
        ])
        self.assertKoakListEqual()


    def test_cond_void_cond(self):
        self.stdin_append([
            'import "../examples/std";'
            "if putchar('a') putcln('a') else putcln('b');"
        ])
        self.assertKoakLastErrorContain("Can't cast type \"void\" to type \"bool\"")


class VariableTests(JITCustomTestCase):
    def test_basic_assign(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func() -> void { let x = 'a'; putcln(x); }"
            "func();"
        ])
        self.stdout_expected([
            "a"
        ])
        self.assertKoakListEqual()

    def test_shadow_param(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(x: double) -> void { let x = 'a'; putcln(x); }"
            "func(2.5);"
        ])
        self.stdout_expected([
            "a"
        ])
        self.assertKoakListEqual()

    def test_basic_mut(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func() -> void { let mut x = 'a'; putcln(x); x = 'b'; putcln(x); }"
            "func();"
        ])
        self.stdout_expected([
            "a",
            "b",
        ])
        self.assertKoakListEqual()

    def test_reassign_const(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func() -> void { let x = 'a'; putcln(x); x = 'b'; putcln(x); }"
        ])
        self.assertKoakLastErrorContain("Can't re-assign the constant variable \"x\"")

    def test_reassign_const_param(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(x: char) -> void { putcln(x); x = 'b'; putcln(x); }"
        ])
        self.assertKoakLastErrorContain("Can't re-assign the constant variable \"x\"")

    def test_reassign_mut_param(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(mut x: char) -> void { putcln(x); x = 'b'; putcln(x); }"
            "func('a');"
        ])
        self.stdout_expected([
            "a",
            "b",
        ])
        self.assertKoakListEqual()

    def test_reassign_rvalue(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func() -> void { let mut x = 5; x + 1 = 'a'; }"
        ])
        self.assertKoakLastErrorContain("Can't assign an r-value expression")

    def test_assign_op(self):
        self.stdin_append([
            'import "../examples/std";'
            "def func(mut x: char) -> void { putcln('0' + x); x += 2; putcln('0' + x); x -= 3; putcln('0' + x); x *= 2; putcln('0' + x); x /= 2; putcln('0' + x); x %= 3; putcln('0' + x); }"
            "func(5);"
        ])
        self.stdout_expected([
            "5",
            "7",
            "4",
            "8",
            "4",
            "1",
        ])
        self.assertKoakListEqual()


class SeeminglyRandomButTheyArentTests(JITCustomTestCase):
    def test_bool_equality(self):
        self.stdin_append([
            "true > 0;"
            "false > 0;"
        ])
        self.stdout_expected([
            "=> true",
            "=> false",
        ])
        self.assertKoakListEqual()

    def test_multiple_exprs(self):
        self.stdin_append("1;2;3+4;10")
        self.stdout_expected([
            "=> 1",
            "=> 2",
            "=> 7",
            "=> 10",
        ])
        self.assertKoakListEqual()

    def test_eq_void(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "putchar('0')==putchar('0');"
        ])
        self.assertKoakLastErrorContain("Invalid binary operator for type")

    def test_le_void(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "putchar('0')<=putchar('0');"
        ])
        self.assertKoakLastErrorContain("Invalid binary operator for type")

    def test_ge_void(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "putchar('0')>=putchar('0');"
        ])
        self.assertKoakLastErrorContain("Invalid binary operator for type")

    def test_ne_void(self):
        self.stdin_append([
            "extern putchar(c: char) -> void;",
            "putchar('0')!=putchar('0');"
        ])
        self.assertKoakLastErrorContain("Invalid binary operator for type")

    def test_eq_bool(self):
        self.stdin_append([
            "true==true;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_le_bool(self):
        self.stdin_append([
            "true<=true;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_ge_bool(self):
        self.stdin_append([
            "true>=false;"
        ])
        self.assertKoakLastOutEqual("=> true\n")

    def test_ne_bool(self):
        self.stdin_append([
            "true!=false;"
        ])
        self.assertKoakLastOutEqual("=> true\n")


class ShiftOperatorTest(JITCustomTestCase):

    def _bool_to_str(self, b: bool) -> str:
        return "=> true" if b else "=> false"

    def test_bitshift_int_shl(self):
        self.stdin_append([
            "3 << 3;",
            "4 << 3;",
            "6 << 3;",
        ])
        self.stdout_expected([
            self._to_res(3 << 3),
            self._to_res(4 << 3),
            self._to_res(6 << 3),
        ])
        self.assertKoakListEqual()

    def test_bitshift_int_shr(self):
        self.stdin_append([
            "3 >> 3;",
            "4 >> 3;",
            "6 >> 3;",
        ])
        self.stdout_expected([
            self._to_res(3 >> 3),
            self._to_res(4 >> 3),
            self._to_res(6 >> 3),
        ])
        self.assertKoakListEqual()

    def test_bitshift_double_shl(self):
        self.stdin_append([
            "3.0 << 3.0;",
            "4.0 << 3.0;",
            "6.0 << 3.0;",
        ])
        self.assertKoakListEqual()

    def test_bitshift_double_shr(self):
        self.stdin_append([
            "3.0 >> 3.0;",
            "4.0 >> 3.0;",
            "6.0 >> 3.0;",
        ])
        self.assertKoakNeedError()

    def test_bitshift_bool_shl(self):
        self.stdin_append([
            ## "true << true;",
            ## "false << true;",
            ## "true << false;",
            "false << false;",
        ])
        self.stdout_expected([
            ##self._bool_to_str(True << True),
            ##self._bool_to_str(False << True),
            ##self._bool_to_str(True << False),
            self._bool_to_str(False << False),
        ])
        self.assertKoakListEqual()

    def test_bitshift_bool_shr(self):
        self.stdin_append([
            ## "true >> true;",
            ## "false >> true;",
            "true >> false;",
            "false >> false;",
        ])
        self.stdout_expected([
            ## self._bool_to_str(True >> True),
            ## self._bool_to_str(False >> True),
            self._bool_to_str(True >> False),
            self._bool_to_str(False >> False),
        ])
        self.assertKoakListEqual()


class BitwiseOperatorTest(JITCustomTestCase):

    def test_bitwise_xor(self):
        self.stdin_append([
            "3 ^ 3;",
            "4 ^ 3;",
            "6 ^ 3;",
        ])
        self.stdout_expected([
            self._to_res(3 ^ 3),
            self._to_res(4 ^ 3),
            self._to_res(6 ^ 3),
        ])
        self.assertKoakListEqual()

    def test_bitwise_and(self):
        self.stdin_append([
            "1 & 2;",
            "1 & 3;",
            "6 & 3;",
        ])
        self.stdout_expected([
            self._to_res(1 & 2),
            self._to_res(1 & 3),
            self._to_res(6 & 3),
        ])
        self.assertKoakListEqual()

    def test_bitwise_or(self):
        self.stdin_append([
            "3 | 3;",
            "4 | 3;",
            "6 | 3;",
        ])
        self.stdout_expected([
            self._to_res(3 | 3),
            self._to_res(4 | 3),
            self._to_res(6 | 3),
        ])
        self.assertKoakListEqual()

    def test_bitwise_or_err_double(self):
        self.stdin_append(["3.0 | 3;"])
        self.assertKoakNeedError()

    def test_bitwise_or_err_void(self):
        self.def_putchar()
        self.stdin_append([
            "putchar('e') | 3;"
        ])
        self.assertKoakNeedError()

    def test_bitwise_and_err_double(self):
        self.stdin_append(["3.0 & 3;"])
        self.assertKoakNeedError()

    def test_bitwise_and_err_void(self):
        self.def_putchar()
        self.stdin_append([
            "putchar('e') & 3;"
        ])
        self.assertKoakNeedError()

    def test_bitwise_xor_err_double(self):
        self.stdin_append(["3.0 ^ 3;"])
        self.assertKoakNeedError()

    def test_bitwise_xor_err_void(self):
        self.def_putchar()
        self.stdin_append([
            "putchar('e') ^ 3;"
        ])
        self.assertKoakNeedError()

    def test_neg_err_void(self):
        self.def_putchar()
        self.stdin_append([
            "!putchar('e')"
        ])
        self.assertKoakNeedError()

    def test_not_err_void(self):
        self.def_putchar()
        self.stdin_append([
            "!putchar('e')"
        ])
        self.assertKoakNeedError()


if __name__ == "__main__":
    TextTestRunner.resultclass = ColorTextTestResult
    main(module=None, testRunner=TextTestRunner())
