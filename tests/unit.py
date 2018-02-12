import uuid
from subprocess import Popen, PIPE
from unittest import TestCase, main

BINARY = "../target/debug/koak"


def popen_koak(args: list, stdin=None):
    """
    Helper Popen Koak
    :param args:
    :param stdin:
    :return:
    """
    return Popen([BINARY, "-t"] + args, stdout=PIPE, stderr=PIPE, stdin=stdin)


def popen_koakfile(filename: str, args=None):
    """
    Helper Popen Koak with filename
    :param filename:
    :param args:
    :return:
    """
    if args is None:
        args = []
    return popen_koak([filename] + args)


def popen_koakfile_with_lines(lines: list, args=None):
    """
    Helper Popen file input with Line array
    :param lines:
    :param args:
    :return:
    """
    if args is None:
        args = []
    filename = "/tmp/" + str(uuid.uuid4()) + ".tmp"
    with open(filename, 'w') as tmp:
        for line in lines:
            tmp.write(line + "\n")
    return popen_koakfile(filename, args)


def popen_koakpipe_with_lines(lines: list, args=None):
    """
    Helper Popen Pipe with Line array
    :param lines:
    :param args:
    :return:
    """
    if args is None:
        args = []
    filename = "/tmp/" + str(uuid.uuid4()) + ".tmp"
    with open(filename, 'w') as tmp:
        for line in lines:
            tmp.write(line + "\n")
    with open(filename, 'r') as tmp:
        return popen_koak(args, tmp)
    raise RuntimeError()


def extract_lines_from_std(buffer) -> list:
    """
    Extract lines of buffer by del = \n without the empty last line
    :param buffer:
    :return: list of lines
    """
    return list(filter(bool, buffer.split('\n')))


class LexerTest(TestCase):
    """
    LexerTest implementation
    """

    def test_Unknown_char_1(self):
        p = popen_koakfile_with_lines([
            "'"
        ], ["-l"])

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertEqual(True, str(err.split('\n')).__contains__("Unknown char '''"))

    def test_Unknown_char_2(self):
        p = popen_koakfile_with_lines([
            "def x(&) x * x"
        ], ["-l"])

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertEqual(True, str(err.split('\n')).__contains__("Unknown char '&'"))

    def test_Top_Level_Tokens(self):
        p = popen_koakfile_with_lines([
            "def x(&) x * x"
        ], ["-l"])

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertEqual(True, str(err.split('\n')).__contains__("Unknown char '&'"))

    def test_Similar_to_keywords_tokens(self):
        p = popen_koakfile_with_lines([
            "externa bdef"
        ], ["-l"])

        expected_out = [
            "Identifier(\"externa\")",
            "Identifier(\"bdef\")",
        ]

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertListEqual(expected_out, extract_lines_from_std(out))


class ParserTest(TestCase):
    """
    ParserTest implementation
    """

    def test_extern_prototype_ok_empty_args(self):
        p = popen_koakfile_with_lines([
            "extern x()"
        ], ["-p"])

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertEqual("ExternProto(x, [])", extract_lines_from_std(out)[-1])

    def test_extern_prototype_ok_1_args(self):
        p = popen_koakfile_with_lines([
            "extern x(x)"
        ], ["-p"])

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertEqual("ExternProto(x, [\"x\"])", extract_lines_from_std(out)[-1])

    def test_extern_prototype_ok_2_args(self):
        p = popen_koakfile_with_lines([
            "extern x(x y)"
        ], ["-p"])

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertEqual("ExternProto(x, [\"x\", \"y\"])", extract_lines_from_std(out)[-1])


class PipeTest(TestCase):
    """Test case utilisé pour tester les fonctions du module 'random'."""

    def test_choice(self):
        p = popen_koakpipe_with_lines([
            "def square(x) x * x",
            "square(5)"
        ])

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertEqual("=> 25", extract_lines_from_std(out)[-1])

    def test_bug_fct_return(self):
        p = popen_koakpipe_with_lines([
            "def square(x) x * x",
            "square(5)",
            "x = 1",
            "square(5)"
        ])

        p.wait()

        stdout, stderr = p.communicate()
        out = stdout.decode("ascii")
        err = stderr.decode("ascii")

        self.assertEqual("=> 25", extract_lines_from_std(out)[-1])


if __name__ == "__main__":
    main()
