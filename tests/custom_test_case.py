from enum import Enum, unique, auto
from unittest import TestCase, main

from helper.formater import extract_lines_from_std, last
from helper.popen import *


class Orientation(Enum):
    START_FIRST = auto()
    START_END = auto()


class InputType(Enum):
    FILE = auto()
    PIPE = auto()


class Stream(Enum):
    STDIN = auto()
    STDOUT = auto()
    STDERR = auto()
    STDOUT_AND_STDERR = auto()


def process_input_list_from_file(input_list: list, args=None) -> (list, list):
    """
    Basic unit test method with two list string without carriage return, from a file
    :rtype: tuple(list, list)
    :return: (list_out, list_err)
    """

    if args is None:
        args = ["-l"]

    p = popen_koakfile_with_lines(input_list, args)

    p.wait()

    stdout, stderr = p.communicate()

    list_out = extract_lines_from_std(stdout.decode("ascii"))
    list_err = extract_lines_from_std(stderr.decode("ascii"))

    return list_out, list_err


def pdg_if_fail(function):
    # noinspection PyArgumentList
    """
    Call method self.std_debug_display() if run test AssertionError is raised
    :param function:
    :return:
    """
    def wrapper(self):
        try:
            function(self)
        except AssertionError as e:
            self.std_debug_display()
            raise e
    return wrapper


class CustomTestCase(TestCase):

    def __init__(self, methodName='runTest'):
        super().__init__(methodName)

    def setUp(self):
        print("[Before] " + self._testMethodName + ": clean env...")
        self.init()

    def init(self):
        self.list_args = ["-t"]
        self.list_stdin = None
        self.list_stdout = None
        self.list_stderr = None
        self.input_type = InputType.FILE
        self.run = False

    def input_type_piped(self):
        self.input_type = InputType.PIPE

    def input_type_file(self):
        self.input_type = InputType.FILE

    def set_list_args(self, args=None):
        if args is None:
            args = ["-t"]
        self.list_args = args

    def std_append(self, std: list, lines: object, endline: str = "\n"):
        if isinstance(lines, list):
            std += list(map((lambda line: line + endline), lines))
        elif isinstance(lines, str):
            std.append(lines + endline)
        else:
            raise TypeError("std_append list or str")

    def stderr_expected(self, lines: object, endline: str = "\n"):
        if self.list_stderr is None:
            self.list_stderr = []
        self.std_append(self.list_stderr, lines, endline)

    def stdout_expected(self, lines: object, endline: str = "\n"):
        if self.list_stdout is None:
            self.list_stdout = []
        self.std_append(self.list_stdout, lines, endline)

    def stdin_append(self, lines: object, endline: str = "\n"):
        if self.list_stdin is None:
            self.list_stdin = []
        self.std_append(self.list_stdin, lines, endline)

    def std_debug_display(self):
        print("===================================DEBUG========================================")
        print("= TEST NAME: " + self._testMethodName)
        print("|= STDIN =>")
        [print(line, end="") for line in self.list_stdin] if self.list_stdin is not None else print("None")
        print(">= STDIN =|")
        print("|= STDOUT =>")
        [print(line, end="") for line in self.list_stdout] if self.list_stdout is not None else print("None")
        print(">= STDOUT =|")
        print("|= STDERR =>")
        [print(line, end="") for line in self.list_stderr] if self.list_stderr is not None else print("None")
        print(">= STDERR =|")
        print("=================================END DEBUG======================================")

    def assertKoakListEqual(self, stream_check: Stream = Stream.STDOUT_AND_STDERR):
        outs, errs = self.runKoak()

        if self.list_stdout is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDOUT):
            self.assertListEqual(self.list_stdout, outs)

        if self.list_stderr is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDERR):
            self.assertListEqual(self.list_stderr, errs)

    def assertKoakLastContain(self, test_out: object, test_error: object,
                              stream_check: Stream = Stream.STDOUT_AND_STDERR):
        outs, errs = self.runKoak()
        lout = last(outs)
        lerr = last(errs)

        if test_out is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDOUT):
            self.assertNotEqual(None, lout)
            self.assertEqual(True, str(lout).__contains__(test_out), msg="Last line stdout must contain: " + str(test_out))

        if test_error is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDERR):
            self.assertNotEqual(None, lerr)
            self.assertEqual(True, str(lerr).__contains__(test_error),  msg="Last line stdout must contain: " + str(test_error))

    def assertKoakLastErrorContain(self, test_error: str):
        self.assertKoakLastContain(None, test_error, Stream.STDERR)

    def assertKoakLastOutContain(self, test_out: str):
        self.assertKoakLastContain(None, test_out, Stream.STDERR)

    def assertKoakLastEqual(self, test_out: object, test_error: object,
                            stream_check: Stream = Stream.STDOUT_AND_STDERR):
        outs, errs = self.runKoak()
        lout = last(outs)
        lerr = last(errs)

        if test_out is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDOUT):
            self.assertNotEqual(None, lout)
            self.assertEqual(test_out, str(lout))

        if test_error is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDERR):
            self.assertNotEqual(None, lerr)
            self.assertEqual(test_error, str(lerr))

    def assertKoakLastErrorEqual(self, test_error: str):
        self.runKoak()
        self.assertKoakLastEqual(None, test_error, Stream.STDERR)

    def assertKoakLastOutEqual(self, test_out: str):
        self.runKoak()
        self.assertKoakLastEqual(None, test_out, Stream.STDOUT)

    def assertKoakZeroError(self):
        self.runKoak()
        self.assertEqual(0,
                         len(self.list_stderr),
                         msg="No output in stderr is required !\nCurrent list errors:\n{0}".format(str(self.list_stderr)))

    def assertKoakNeedError(self):
        self.runKoak()
        self.assertEqual(1,
                         len(self.list_stderr) > 0,
                         msg="No output in stderr is required !\nCurrent list errors:\n{0}".format(str(self.list_stderr)))

    def runKoak(self):
        if not self.run:
            self.list_stdout, self.list_stderr, _, _ = self.process_input_test(self.list_stdin, self.list_args, self.input_type)
        outs = self.list_stdout
        errs = self.list_stderr
        return outs, errs

    def process_input_list_from_pipe(self, input_list: list, args=None) -> (list, list):
        """
        Basic unit test method with two list string without carriage return, from a Pipe
        :rtype: tuple(list, list)
        :return: (list_out, list_err)
        """

        if args is None:
            args = ["-l"]

        p = popen_koakpipe_with_lines(input_list, args)

        p.wait()

        stdout, stderr = p.communicate()

        list_out = extract_lines_from_std(stdout.decode("ascii"))
        list_err = extract_lines_from_std(stderr.decode("ascii"))

        return list_out, list_err

    def process_input_list(self, input_list: list, args=None) -> (list, list, list, list):
        a, b = self.process_input_list_from_pipe(input_list, args)
        c, d = process_input_list_from_file(input_list, args)
        return a, b, c, d

    def process_input_test_from_file(self, input_list: list, args=None) -> (list, list, list, list):
        """
        Basic unit test method to load full pipeline
        :param input_list:
        :param args:
        :return: (list_out, list_err, last_out, last_err)
        """
        list_out, list_err = process_input_list_from_file(input_list, args)
        last_out, last_err = self.both_last_elements(list_out, list_err)
        return list_out, list_err, last_out, last_err

    def process_input_test_from_pipe(self, input_list: list, args=None) -> (list, list, list, list):
        """
        Basic unit test method to load full pipeline
        :param input_list:
        :param args:
        :return: (list_out, list_err, last_out, last_err)
        """
        list_out, list_err = self.process_input_list_from_pipe(input_list, args)
        last_out, last_err = self.both_last_elements(list_out, list_err)
        return list_out, list_err, last_out, last_err

    def process_input_test(self, input_list: list, args=None, input_type=InputType.FILE) -> (list, list, list, list):
        """
        Basic unit test universal method
        :param input_list:
        :param args:
        :param input_type: PIPE or FILE
        :return:
        """
        return self.process_input_test_from_file(input_list, args) \
            if input_type is InputType.FILE \
            else self.process_input_test_from_pipe(input_list, args)

    def both_last_elements(self, l1: list, l2: list) -> (object, object):
        return last(l1), last(l2)

    def assert_both_last_elements(self, l1: list, l2: list) -> (object, object):
        a, b = last(l1), last(l2)
        self.assertEqual(False, a is None or b is None)
