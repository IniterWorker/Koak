from unittest import main

from custom_test_case import CustomTestCase, pdg_if_fail


class JITCustomTestCase(CustomTestCase):

    def init(self):
        super().init()
        self.set_list_args(["-t"])
        self.input_type_piped()


class DefinitionTest(JITCustomTestCase):
    """Test case utilisé pour tester les fonctions du module 'random'."""

    @pdg_if_fail
    def test_choice(self):
        self.stdin_append([
            "def square(x) x * x",
            "square(5)"
        ])
        self.assertKoakLastOutEqual("=> 25\n")

    @pdg_if_fail
    def test_bug_fct_return(self):
        self.stdin_append([
            "def square(x) x * x",
            "square(5)",
            "x = 1",
            "square(5)"
        ])
        self.assertKoakLastOutEqual("=> 25\n")


if __name__ == "__main__":
    main()
