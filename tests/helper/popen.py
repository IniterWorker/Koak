import uuid
from subprocess import Popen, PIPE

BINARY = "../target/debug/koak"


def popen_koak(args: list, stdin=None):
    """
    Helper Popen Koak
    :param args:
    :param stdin:
    :return:
    """
    return Popen([BINARY] + args, stdout=PIPE, stderr=PIPE, stdin=stdin)


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
