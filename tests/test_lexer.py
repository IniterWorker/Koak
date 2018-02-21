from unittest import main

from custom_test_case import CustomTestCase


class CustomLexerTestCase(CustomTestCase):

    def init(self):
        super().init()
        self.set_list_args(["-t", "-l"])


class UnknownCharTest(CustomLexerTestCase):
    """
    Lexer Test Case
    Test basic unknown char in the context
    """

    def test_Unknown_char_1(self):
        self.stdin_append("'")
        # self.assertKoakLastErrorContain("Unknown char '''")
        self.assertKoakLastErrorContain("Invalid literal char")  # since typing char

    def test_Unknown_char_2(self):
        self.stdin_append("def x(&) x * x")
        self.assertKoakLastErrorContain("Unknown char '&'")

    def test_Top_Level_Tokens(self):
        self.stdin_append("def x(&) x * x")
        self.assertKoakLastErrorContain("Unknown char '&'")


class KeywordsTest(CustomLexerTestCase):
    """
    Lexer Test Case
    Test basic keyword
    """

    def test_Similar_to_keywords_tokens(self):
        self.stdin_append("externa bdef")
        self.stdout_expected([
            "Identifier(\"externa\")",
            "Identifier(\"bdef\")",
        ])
        self.assertKoakListEqual()


class DelimiterTest(CustomLexerTestCase):
    """
    Lexer Test Case
    Test basic linear expression
    """

    def test_delimiter_only(self):
        self.stdin_append(";")
        self.stdout_expected([
            "SemiColon"
        ])
        self.assertKoakListEqual()
        self.assertKoakZeroError()

    def test_delimiter_multiple_empty(self):
        self.stdin_append(";;;")
        self.stdout_expected([
            "SemiColon",
            "SemiColon",
            "SemiColon"
        ])
        self.assertKoakListEqual()
        self.assertKoakZeroError()


class CmpOperatorTest(CustomLexerTestCase):
    """
    Lexer Test Case
    Test cmp operator
    """

    def test_equal_basic(self):
        self.stdin_append("1 == 1")
        self.stdout_expected([
            "IntegerLiteral(1)",
            "Operator(Equal)",
            "IntegerLiteral(1)"
        ])
        self.assertKoakListEqual()

    def test_equal_basic(self):
        self.stdin_append("1 == 1")
        self.stdout_expected([
            "IntegerLiteral(1)",
            "Operator(Equal)",
            "IntegerLiteral(1)"
        ])
        self.assertKoakListEqual()


if __name__ == "__main__":
    main()
