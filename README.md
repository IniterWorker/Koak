# Kind Of Alternative Kaleidoscope

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

```
$ cargo run
>> def fib(x) if x < 3 then 1 else fib(x - 1) + fib(x - 2);

define double @fib(double %x) {
entry:
  %cmptmp = fcmp olt double %x, 3.000000e+00
  %casttmp = uitofp i1 %cmptmp to double
  %cond = fcmp one double %casttmp, 0.000000e+00
  br i1 %cond, label %then, label %else

then:                                             ; preds = %entry
  br label %merge

else:                                             ; preds = %entry
  %subtmp = fsub double %x, 1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %subtmp1 = fsub double %x, 2.000000e+00
  %calltmp2 = call double @fib(double %subtmp1)
  %addtmp = fadd double %calltmp, %calltmp2
  br label %merge

merge:                                            ; preds = %else, %then
  %ifphi = phi double [ 1.000000e+00, %then ], [ %addtmp, %else ]
  ret double %ifphi
}

>> fib(40);
102334155
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
  - [ ] Type system (`void`, `char`, `int`, `double` etc.)
  - [ ] Type inference
  - [ ] Strings
- [ ] Execution and compilation
  - [X] JIT Integration
  - [ ] Compilation into object file (.o)
  - [ ] Linking all object file into a final binary
  - [ ] Linking with the c library
