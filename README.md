# Lateral Lisp has moved
[New repository](https://github.com/whetfire/lateral-lang)

# Lateral Lisp

Lateral is a simple dialect of Lisp.
This repository contains a bootstrapping interpreter and the early stages of a
self-hosting compiler.

Lateral is intended to be as simple as possible without sacrificing convinience.
At this point in time, the core library is highly volatile. Expect function
names and behavior to change.

## Examples

```lisp
> (print "Hello World!")
Hello World!
=> nil

> (map inc (list 1 2 3 4 5))
=> (2 3 4 5 6)

> (filter list? (quote (1 (a b) 3 4 (d))))
=> ((a b) (d))
```

### Math

```lisp
> (+ 1 2 3 4)
=> 10
```

## Building

Run the provide Makefile in `Lateral/bootstrap` to build the bootstrapping
interpreter (written in C99). The C interpreter depends on GNU Readline.

`make all`

Python3 is needed to run the tests.

`make tests`

## Current Status

### October 12

The interpreter is now self-hosting. A C99 compiler and a JRE are required.
Here are the steps to reproduce it:

```bash
Lateral/bootstrap $ make
Lateral/bootstrap $ ./a.out jvmclass.lisp
Lateral/bootstrap $ mv Lateral.class ../jvm/
Lateral/bootstrap $ cd ../jvm/
Lateral/jvm $ javac *.java
```

This produces the Lateral interpreter written in Lateral. To show that it
is self hosting, the following command in the repl will produce a functionally
equivalent `Lateral.class` file written to `LateralB.class`.

```bash
Lateral/jvm $ java Helper
user> (include "jvmclass.lisp")
```

If `Lateral.class` is replaced with `LateralB.class` and the previous two
commands are run again, the new `LateralB.class` will be an exact replica of
the old one.

### October 4

The compilation target is now going to be JVM Bytecode. It's more fun than
compiling to C. This also has the side effect of opening up the possibility of
a JIT compiler. 

Code for general compilation is in `compiler.lisp`, while JVM-specific code now
resides in `jvmclass.lisp`. There is functionality to call native functions and
to call other compiled functions.

The only thing left to do is to be able to compile all of the special forms.
Generating bytecode should be straightforward for everything except `let` and
`lambda`. Compiling let involves setting local variables and resolving
bindings in nested let statements. Lambdas are difficult because the JVM is
not designed for first class functions. Anonymous functions are like let
statements, but clojures and passing functions as objects will be interesting
to compile.

## Resources

- [Make a Lisp](https://github.com/kanaka/mal)
- [Crafting Interpreters](https://craftinginterpreters.com/contents.html)

- [JVM Specification (SE11)](https://docs.oracle.com/javase/specs/jvms/se11/html/index.html)

- [Lateral lisp](https://en.wikipedia.org/wiki/Lisp#Types)

## License

This project is released under the GPL v3.0.
