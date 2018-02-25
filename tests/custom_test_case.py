from enum import Enum, unique
import logging
from unittest import TestCase, main, TestResult

from helper.formater import extract_lines_from_std, last
from helper.popen import *


class Orientation(Enum):
    START_FIRST = 1
    START_END = 2


class InputType(Enum):
    FILE = 1
    PIPE = 2


class Stream(Enum):
    STDIN = 1
    STDOUT = 2
    STDERR = 3
    STDOUT_AND_STDERR = 4


def safe_len(o: list) -> int:
    return 0 if o is None else len(o)


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
    logging.basicConfig()
    log = logging.getLogger("LOG")

    def __init__(self, methodName='runTest'):
        super().__init__(methodName)

    def setUp(self):
        """
        Run before each test to setup with self.init method.
        :return:
        """
        self.init()

    def init(self):
        self.list_args = ["-t"]
        self.list_stdin = None
        self.list_stdout = None
        self.list_stderr = None
        self.current_stdout = None
        self.current_stderr = None
        self.input_type = InputType.FILE
        self.run = False

    def input_type_piped(self):
        """
        Set type Pipe
        :return:
        """
        self.input_type = InputType.PIPE

    def input_type_file(self):
        """
        Set type file
        :return:
        """
        self.input_type = InputType.FILE

    def set_list_args(self, args=None):
        """
        Set list args of Koak
        :param args:
        :return:
        """
        if args is None:
            args = ["-t"]
        self.list_args = args

    def std_append(self, std: list, lines: object, endline: str = "\n"):
        """
        Append in list with additional strings list or string
        :param std:
        :param lines:
        :param endline:
        :return:
        """
        if isinstance(lines, list):
            std += list(map((lambda line: line + endline), lines))
        elif isinstance(lines, str):
            std.append(lines + endline)
        else:
            raise TypeError("std_append list or str")

    def stderr_expected(self, lines: object, endline: str = "\n"):
        """
        Append method to add expected output in stderr
        :param lines:
        :param endline:
        """
        if self.list_stderr is None:
            self.list_stderr = []
        self.std_append(self.list_stderr, lines, endline)

    def stdout_expected(self, lines: object, endline: str = "\n"):
        """
        Append method to add expected output in stderr
        :param lines:
        :param endline:
        """
        if self.list_stdout is None:
            self.list_stdout = []
        self.std_append(self.list_stdout, lines, endline)

    def stdin_append(self, lines: object, endline: str = "\n"):
        """
        Append method to add buffer in stdin
        :param lines:
        :param endline:
        """
        if self.list_stdin is None:
            self.list_stdin = []
        self.std_append(self.list_stdin, lines, endline)

    def std_debug_display(self):
        pass

    def debug(self):
        super().debug()

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
            self.assertEqual(True, str(lout).__contains__(test_out),
                             msg="Last line stdout must contain: " + str(test_out))

        if test_error is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDERR):
            self.assertNotEqual(None, lerr)
            self.assertEqual(True, str(lerr).__contains__(test_error),
                             msg="Last line stdout must contain: " + str(test_error))

    def assertKoakLastErrorContain(self, test_error: str):
        self.assertKoakLastContain(None, test_error, Stream.STDERR)

    def assertKoakLastOutContain(self, test_out: str):
        self.assertKoakLastContain(test_out, None, Stream.STDERR)

    def assertKoakLastEqual(self, test_out: object, test_error: object,
                            stream_check: object = Stream.STDOUT_AND_STDERR) -> object:
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

    def _trace_custom(self) -> str:
        padding = "  "
        trace = "\n\n"
        trace += "Koak trace (stdin, stdout, stderr) :\n"
        trace += "STDIN >\n"
        trace += "".join(map(lambda x: padding + x, self.list_stdin)) + "\n" if self.list_stdin is not None else "\n"
        trace += "STDOUT >\n"
        trace += "".join(
            map(lambda x: padding + x, self.current_stdout)) + "\n" if self.current_stdout is not None else "\n"
        trace += "STDOUT (expected) >\n"
        trace += "".join(map(lambda x: padding + x, self.list_stdout)) + "\n" if self.list_stdout is not None else "\n"
        trace += "STDERR >\n"
        trace += "".join(
            map(lambda x: padding + x, self.current_stderr)) + "\n" if self.current_stderr is not None else "\n"
        trace += "STDERR (expected) >\n"
        trace += "".join(map(lambda x: padding + x, self.list_stderr)) + "\n" if self.list_stderr is not None else "\n"
        return trace

    def _formatMessage(self, msg, standardMsg):
        """
        Adding additional pieces of information in debug stack
        :param msg:
        :param standardMsg:
        :return: str
        """
        return self._trace_custom() + "\n" + super()._formatMessage(msg, standardMsg)

    def assertKoakLastErrorEqual(self, test_error: str):
        self.runKoak()
        self.stderr_expected(test_error)
        self.assertKoakLastEqual(None, test_error, Stream.STDERR)

    def assertKoakLastOutEqual(self, test_out: str):
        self.runKoak()
        self.stdout_expected(test_out)
        self.assertKoakLastEqual(test_out, None, Stream.STDOUT)

    def assertKoakZeroError(self):
        outs, errs = self.runKoak()
        self.assertEqual(True,
                         safe_len(errs) == 0,
                         msg="No output in stderr is required !\nCurrent list errors:\n{0}".format(
                             str(errs)))

    def assertKoakZeroOut(self):
        outs, errs = self.runKoak()
        self.assertEqual(True,
                         safe_len(outs) == 0,
                         msg="No output in stdout is required !\nCurrent list outs:\n{0}".format(
                             str(errs)))

    def assertKoakZeroAll(self):
        self.assertKoakZeroError()
        self.assertKoakZeroOut()

    def assertKoakNeedError(self):
        outs, errs = self.runKoak()
        self.assertEqual(True,
                         safe_len(errs) > 0,
                         msg="No output in stderr is required !\nCurrent list errors:\n{0}".format(
                             str(errs)))

    def runKoak(self):
        outs, errs, _, _ = self.process_input_test(self.list_stdin, self.list_args, self.input_type)
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

        if self.current_stdout is None or self.current_stdout is None:
            # Cache
            self.current_stderr = stderr.decode("ascii")
            self.current_stdout = stdout.decode("ascii")

            self.current_stdout = extract_lines_from_std(self.current_stdout)
            self.current_stderr = extract_lines_from_std(self.current_stderr)

        return self.current_stdout, self.current_stderr

    def process_input_list_from_file(self, input_list: list, args=None) -> (list, list):
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

        if self.current_stdout is None or self.current_stdout is None:
            # Cache
            self.current_stderr = stderr.decode("ascii")
            self.current_stdout = stdout.decode("ascii")

            self.current_stdout = extract_lines_from_std(self.current_stdout)
            self.current_stderr = extract_lines_from_std(self.current_stderr)

        return self.current_stdout, self.current_stderr

    def process_input_list(self, input_list: list, args=None) -> (list, list, list, list):
        a, b = self.process_input_list_from_pipe(input_list, args)
        c, d = self.process_input_list_from_file(input_list, args)
        return a, b, c, d

    def process_input_test_from_file(self, input_list: list, args=None) -> (list, list, list, list):
        """
        Basic unit test method to load full pipeline
        :param input_list:
        :param args:
        :return: (list_out, list_err, last_out, last_err)
        """
        list_out, list_err = self.process_input_list_from_file(input_list, args)
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
        """
        Grep both last elements
        :param l1:
        :param l2:
        :return:
        """
        return last(l1), last(l2)

    def assert_both_last_elements(self, l1: list, l2: list) -> (object, object):
        """
        AssertEqual on both last elements
        :param l1:
        :param l2:
        :return:
        """
        a, b = last(l1), last(l2)
        self.assertEqual(False, a is None or b is None)

    def assertKoakListContain(self, test_out: object, test_error: object, check_contain: bool, cast_sensitive: bool,
                              stream_check: Stream = Stream.STDOUT_AND_STDERR):
        """
        Assert if one or more line contain search string
        in stdout
        :param cast_sensitive:
        :param check_contain:
        :param test_out:
        :param stream_check:
        :param test_error:
        :param search:
        :return:
        """
        outs, errs = self.runKoak()

        sentence = ""

        if check_contain:
            sentence += "Line contain "
        else:
            sentence += "Line not contain "

        if not cast_sensitive:
            outs = list(map(lambda x: str(x).lower(), outs))
            errs = list(map(lambda x: str(x).lower(), errs))
            if test_out is not None:
                test_out = str(test_out).lower()
            if test_error is not None:
                test_error = str(test_error).lower()
            sentence += " (CI) "
        else:
            sentence += " (CS) "
        sentence += " {} in {} "

        if test_out is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDOUT):
            for out in outs:
                self.assertEqual(check_contain, str(test_out).__contains__(out),
                                 msg=sentence.format(test_out, out))

        if test_error is not None \
                and (stream_check is Stream.STDOUT_AND_STDERR
                     or stream_check is Stream.STDERR):
            for err in errs:
                self.assertEqual(False, str(test_error).__contains__(err),
                                 msg=sentence.format(test_error, err))

    def assertKoakListOutContain(self, search: str, case_sensitive: bool = False):
        """
        Assert if one or more line contain search string
        in stdout
        :param case_sensitive:
        :param search:
        :return:
        """
        self.assertKoakListContain(search, None, True, case_sensitive, Stream.STDOUT)

    def assertKoakListErrContain(self, search: str, case_sensitive: bool = False):
        """
        Assert if one or more line contain search string
        in stderr
        :param case_sensitive:
        :param search:
        :return:
        """
        self.assertKoakListContain(None, search, True, case_sensitive, Stream.STDERR)

    def assertKoakListOutNotContain(self, search: str, case_sensitive: bool = False):
        """
        Assert if one or more line contain search string
        in stdout
        :param case_sensitive:
        :param search:
        :return:
        """
        self.assertKoakListContain(search, None, False, case_sensitive, Stream.STDOUT)

    def assertKoakListErrNotContain(self, search: str, case_sensitive: bool = False):
        """
        Assert if one or more line contain search string
        in stderr
        :param case_sensitive:
        :param search:
        :return:
        """
        self.assertKoakListContain(None, search, False, case_sensitive, Stream.STDERR)

