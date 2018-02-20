# Kind Of Alternative Kaleidoscope [![Build Status](https://travis-ci.org/Arignir/koak.svg?branch=master)](https://travis-ci.org/Arignir/koak)


This is (kind of) a Kaleidoscope compiler, written in Rust.

# Build Dependencies

* rustup, with the 1.25.0 nightly toolchain.
* python 3.6
* llvm with debug assertions enabled

# Building the compiler

If you don't want ton install our build dependencies, you can use our docker image (`arignir/llvm_debug`):

```bash
./docker.sh
```

The source code will be located in the `/koak` directory of the container.

The compilator can be compiled with:

```bash
cargo build
```

# Running the compiler

The easiest way to run the compiler is using it's execution engine. You can do so with the following command:

```bash
cargo run
```

# Examples

```koak
#
# Computes the x'th fibonacci number
#

def fib(x: double) -> double
    if x < 3.0 then
        1.0
    else
        fib(x - 1.0) + fib(x - 2.0)
;
```

```llvm
$ cargo run
>> import "examples/fib";
define double @fib(double %x) {
entry:
  %fcmptmp = fcmp olt double %x, 3.000000e+00
  %cond = icmp ne i1 %fcmptmp, false
  br i1 %cond, label %then, label %else

then:                                             ; preds = %entry
  br label %merge

else:                                             ; preds = %entry
  %fsubtmp = fsub double %x, 1.000000e+00
  %calltmp = call double @fib(double %fsubtmp)
  %fsubtmp1 = fsub double %x, 2.000000e+00
  %calltmp2 = call double @fib(double %fsubtmp1)
  %faddtmp = fadd double %calltmp, %calltmp2
  br label %merge

merge:                                            ; preds = %else, %then
  %ifphi = phi double [ 1.000000e+00, %then ], [ %faddtmp, %else ]
  ret double %ifphi
}

>> fib(42);
=> 267914296
```

# Roadmap

- [X] Lexer
- [X] Parser
- [X] Code generation abstraction
- [X] Nice error reporting
- [ ] Language
  - [X] Local & extern function definition
  - [X] Expression evaluation (basic maths)
  - [X] Unary operators
  - [X] Binary operators
  - [X] Control flow (`if`/`then`/`else`)
  - [ ] Loops (`for`/`while`)
  - [ ] Mutable variables
  - [X] Type system (`void`, `bool`, `char`, `int`, `double` etc.)
  - [ ] Strings
  - [X] Module system (`import`)
- [ ] Execution and compilation
  - [X] JIT Integration
  - [ ] Compilation into object file (.o)
  - [ ] Linking all object file into a final binary
  - [ ] Linking with the c library
