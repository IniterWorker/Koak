from unittest import main

from custom_test_case import CustomTestCase, InputType, pdg_if_fail


class CustomLexerTestCase(CustomTestCase):

    def init(self):
        super().init()
        self.set_list_args(["-t", "-l"])


class UnknownCharTest(CustomLexerTestCase):
    """
    Test basic unknown char in the context
    """

    @pdg_if_fail
    def test_Unknown_char_1(self):
        self.stdin_append("'")
        self.assertKoakLastErrorContain("Unknown char '''")

    @pdg_if_fail
    def test_Unknown_char_2(self):
        self.stdin_append("def x(&) x * x")
        self.assertKoakLastErrorContain("Unknown char '&'")

    @pdg_if_fail
    def test_Top_Level_Tokens(self):
        self.stdin_append("def x(&) x * x")
        self.assertKoakLastErrorContain("Unknown char '&'")


class KeywordsTest(CustomLexerTestCase):
    """
    Test basic keyword
    """

    @pdg_if_fail
    def test_Similar_to_keywords_tokens(self):
        self.stdin_append("externa bdef")
        self.stdout_expected([
            "Identifier(\"externa\")",
            "Identifier(\"bdef\")",
        ])
        self.assertKoakListEqual()


if __name__ == "__main__":
    main()
