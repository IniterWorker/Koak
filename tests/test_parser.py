from unittest import main

from custom_test_case import CustomTestCase, pdg_if_fail


class ParserCustomTestCase(CustomTestCase):

    def init(self):
        super().init()
        self.set_list_args(["-t", "-p"])


class ParserTest(ParserCustomTestCase):
    """
    ParserTest implementation
    """

    @pdg_if_fail
    def test_extern_prototype_ok_empty_args(self):
        self.stdin_append("extern x()")
        self.stdout_expected([
            "ExternProto(x, [])",
        ])
        self.assertKoakListEqual()

    @pdg_if_fail
    def test_extern_prototype_ok_1_args(self):
        self.stdin_append("extern x(x)")
        self.stdout_expected([
            "ExternProto(x, [\"x\"])",
        ])
        self.assertKoakListEqual()

    @pdg_if_fail
    def test_extern_prototype_ok_2_args(self):
        self.stdin_append("extern x(x y)")
        self.stdout_expected([
            "ExternProto(x, [\"x\", \"y\"])",
        ])
        self.assertKoakListEqual()


if __name__ == "__main__":
    main()
