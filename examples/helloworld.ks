#
# Prints Hello World on screen.
#
# Because Koak doesn't support strings yet, we have to print it char by char.
#

import "libc";

def hello_world() -> void {
    putchar('H');
    putchar('e');
    putchar('l');
    putchar('l');
    putchar('o');
    putchar(',');
    putchar(' ');
    putchar('W');
    putchar('o');
    putchar('r');
    putchar('l');
    putchar('d');
    putchar('!');
    putchar('\n');
}

hello_world();
