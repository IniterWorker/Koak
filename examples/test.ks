import "std";

def func(mut x: char) -> void { putcln('0' + x); x += 2; putcln('0' + x); x -= 3; putcln('0' + x); x *= 2; putcln('0' + x); x /= 2; putcln('0' + x); x %= 3; putcln('0' + x); }

#def func(mut x: double) -> void {
#    x %= x;
#    putcln('0' + x);
#}
#
