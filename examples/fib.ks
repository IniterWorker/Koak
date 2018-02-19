#
# Computes the x'th fibonacci number
#

def fib(x: double) -> double
    if x < 3.0 then
        1.0
    else
        fib(x - 1.0) + fib(x - 2.0)
;

fib(42.0); # Computes the 40th number
