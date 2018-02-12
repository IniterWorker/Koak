#
# Quick python framework to make integration tests
#

from colors import Color, print_color

import subprocess
import os
import sys
import itertools

BINARY = os.path.abspath(os.path.dirname(sys.argv[0])) + "/../target/debug/koak"
TMP_INPUT = '/tmp/koak-input'
TMP_STDOUT = '/tmp/koak-out'
TMP_STDERR = '/tmp/koak-err'

TEST_TOTAL=0
TEST_OK=0

class Test:
    def __init__(self, name):
        global TEST_TOTAL

        self.name = name
        self.input = ""
        self.stderr = []
        self.stdout = []
        self.args = []
        TEST_TOTAL += 1

    def run(self):
        global TEST_OK

        print("Running test {}... ".format(self.name), end='')
        # Write input file
        with open(TMP_INPUT, 'w') as out:
            out.write(self.input)

        # Open output files
        fout = open(TMP_STDOUT, 'w')
        ferr = open(TMP_STDERR, 'w')

        # Invoke Koak
        cmd = [ BINARY, TMP_INPUT , "-t" ]
        cmd += self.args
        code = subprocess.call(cmd, stdout=fout, stderr=ferr)
        if self.__compare_file(TMP_STDOUT, self.stdout) and self.__compare_file(TMP_STDERR, self.stderr):
            TEST_OK += 1
            print_color(Color.GREEN, "OK")

    def add_arg(self, args):
        self.args += [ args ]
        return self

    def set_input(self, input):
        self.input = input
        return self

    def expect_syntax_error(self, row, col, err):
        self.stderr += [ ("Syntax Error at line {}, column {}: {}\n".format(row, col, err)) ]
        return self

    def expect_output(self, line):
        self.stdout += [ (line + '\n') ]
        return self

    def __compare_file(self, file, expected):
        with open(file, 'r') as f:
            a = set(f)
            b = set(expected)

            a.discard('\n') # Discard empty lines
            b.discard('\n')

            # Compare differences and pretty-print them
            if len(a.symmetric_difference(b)) != 0:
                print_color(Color.RED, "\n\nDifferences detected!\n")
                for l1, l2 in itertools.zip_longest(a, b, fillvalue=''):
                    print("{} | {}".format(pretty(l1), pretty(l2)))
                print_color(Color.YELLOW, "\nStdout from Koak:")
                with open(TMP_STDOUT, 'r') as f:
                    for line in f:
                        print(">", line, end='')
                print_color(Color.YELLOW, "\nStderr from Koak:")
                with open(TMP_STDERR, 'r') as f:
                    for line in f:
                        print(">", line, end='')
                print("")
                return False
            return True
        return False

class LexerTest(Test):
    def __init__(self, name):
        super(LexerTest, self).__init__("Lexer[" + name + "]")
        self.add_arg("-l")

class ParserTest(Test):
    def __init__(self, name):
        super(ParserTest, self).__init__("Parser[" + name + "]")
        self.add_arg("-p")


def get_nb_ok_tests():
    global TEST_OK
    return TEST_OK

def get_nb_total_tests():
    global TEST_TOTAL
    return TEST_TOTAL

def pretty(x):
    if x.endswith("\n"):
        x = x[:-1]
    if x == '':
        x = '<blank>'
    return x.ljust(30)