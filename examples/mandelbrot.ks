#
# Mandelbrot set ascii-graph
#

import "std";

def print_density(d: double) -> void {
    if d > 8.0
        putc(' ') # ' '
    else if d > 4.0
        putc('.') # '.'
    else if d > 2.0
        putc('+') # '+'
    else
        putc('*') # '*'
}

# Determine whether the specific location diverges.
# Solve for z = z^2 + c in the complex plane.
def mandel_converger(real: double, imag: double, iters: double, creal: double, cimag: double) -> double {
    if iters > 255.0
        iters
    else if (real*real + imag*imag > 4)
        iters
    else
        mandel_converger(real * real - imag * imag + creal, 2 * real * imag + cimag, iters + 1, creal, cimag)
}

# Return the number of iterations required for the iteration to escape
def mandel_converge(real: double, imag: double) -> double {
    mandel_converger(real, imag, 0, real, imag)
}

# Compute and plot the mandelbrot set with the specified 2 dimensional range
# info.
def mandel_help(xmin: double, xmax: double, xstep: double, ymin: double, ymax: double, ystep: double) -> void {
    for y = ymin, y < ymax, ystep in {
        for x = xmin, x < xmax, xstep in {
            print_density(mandel_converge(x,y));
        };
        putc('\n');
    }
}

# mandel - This is a convenient helper function for plotting the mandelbrot set
# from the specified position with the specified Magnification.
def mandel(realstart: double, imagstart: double, realmag: double, imagmag: double) -> void {
    mandel_help(realstart, realstart + realmag * 78, realmag, imagstart, imagstart + imagmag * 40, imagmag);
}

mandel(-2.3, -1.3, 0.05, 0.07);
