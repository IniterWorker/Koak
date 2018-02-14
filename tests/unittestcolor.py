from unittest import TextTestResult, TextTestRunner, main, TestResult

_COLOR = {'green': "\x1b[32;01m",
          'red': "\x1b[31;01m",
          'reset': "\x1b[0m"
          }


def red_str(text):
    """Return red text."""
    global _COLOR
    return _COLOR['red'] + text + _COLOR['reset']


def green_str(text):
    """Return green text."""
    global _COLOR
    return _COLOR['green'] + text + _COLOR['reset']


class ColorTextTestResult(TextTestResult):
    """Colored version."""

    def addSuccess(self, test):
        if self.showAll:
            self.stream.writeln(green_str("Ok\t"))
        elif self.dots:
            self.stream.write(green_str('.'))
        TestResult.addSuccess(self, test)

    def addError(self, test, err):
        TestResult.addError(self, test, err)
        if self.showAll:
            self.stream.writeln(red_str("ERROR\t"))
        elif self.dots:
            self.stream.write(red_str('E'))

    def addFailure(self, test, err):
        TestResult.addFailure(self, test, err)
        if self.showAll:
            self.stream.writeln(red_str("FAIL\t"))
        elif self.dots:
            self.stream.write(red_str('F'))

    def printErrorList(self, flavour, errors):
        for test, err in errors:
            self.stream.writeln(self.separator1)
            self.stream.writeln("%s: %s" % (red_str(flavour),
                                            self.getDescription(test)))
            self.stream.writeln(self.separator2)
            self.stream.writeln("%s" % err)

    def _exc_info_to_string(self, err, test):
        return super()._exc_info_to_string(err, test)


if __name__ == "__main__":
    runner = TextTestRunner
    runner.resultclass = ColorTextTestResult
    main(module=None, testRunner=runner)
