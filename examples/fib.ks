#
# Computes the x'th fibonacci number
#

def fib(x: int) -> int
    if x < 3 then
        1
    else
        fib(x - 1) + fib(x - 2)
;

fib(40); # Computes the 40th number
