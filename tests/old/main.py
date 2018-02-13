#!/usr/bin/env python3

from colors import Color, print_color
from framework import LexerTest, ParserTest, get_nb_ok_tests, get_nb_total_tests

def main():
    print_color(Color.BLUE, "Running Koak's tests...\n")

    LexerTest("Unknown char 1") \
        .set_input("'") \
        .expect_syntax_error(1, 1, "Unknown char '''") \
        .run()

    LexerTest("Unknown char 2") \
        .set_input("\n\ndef x(&) x * x") \
        .expect_syntax_error(3, 7, "Unknown char '&'") \
        .run()

    LexerTest("Top level tokens") \
        .set_input('extern def') \
        .expect_output('Extern') \
        .expect_output('Def') \
        .run()

    LexerTest("Similar-to-keywords tokens") \
        .set_input('externa bdef') \
        .expect_output('Identifier("externa")') \
        .expect_output('Identifier("bdef")') \
        .run()

    print("") # Separate lexer from parser tests

    ParserTest("Extern Prototype OK - Empty args") \
        .set_input('extern x()') \
        .expect_output('ExternProto(x, [])') \
        .run()

    ParserTest("Extern Prototype OK - 1 arg") \
        .set_input('extern x(x)') \
        .expect_output('ExternProto(x, ["x"])') \
        .run()

    ParserTest("Extern Prototype OK - 2 arg") \
        .set_input('extern x(x y)') \
        .expect_output('ExternProto(x, ["x", "y"])') \
        .run()

    # Second error is debatable. It's the consequence of how the first error is handled.
    ParserTest("Extern Prototype KO - missing name") \
        .set_input('extern ()') \
        .expect_syntax_error(1, 8, "Function name was expected in a prototype") \
        .expect_syntax_error(1, 9, "An expression was expected") \
        .run()

    print_color(Color.BLUE, "\nDone! {} / {} tests passed.".format(get_nb_ok_tests(), get_nb_total_tests()))

if __name__ == '__main__':
    main()