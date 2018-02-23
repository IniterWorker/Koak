#
# Sort-of standard library for the Koak
#

import "libc";

# Print the given character
def putc(c: char) -> void {
    putchar(c);
}

# Print the given character followed by a \n
def putcln(c: char) -> void {
    putc(c);
    putc('\n')
}

# Cast the given argument to bool
def to_bool(x: bool) -> bool {
    x
}

# Cast the given argument to char
def to_char(x: char) -> char {
    x
}

# Cast the given argument to int
def to_int(x: int) -> int {
    x
}

# Cast the given argument to double
def to_double(x: double) -> double {
    x
}
