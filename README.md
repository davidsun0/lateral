# Lateral Lisp

Lateral is a simple dialect of Lisp.
This repository contains a bootstrapping interpreter and the early stages of a
self-hosting compiler.

Lateral is intended to be as simple as possible without sacrificing
convinience. However, at this point, I would not consider anything in the
core library to be set in stone at this point; programs will break as things
change.

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

Run the provide Makefile.
The interpreter depends on GNU Readline.

`make all`

Python3 is needed to run the tests.

`make tests`

## Current Status

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

### September 9

Code destructuring is mostly complete. Compiling to C is coming up on the
horizon. Here is an example of the current code compiling the distance formula:

```lisp
> (def distance-code (quote (sqrt (+ (sq (- x1 x2)) (sq (- y1 y2))))))
> (use-define (destr2 distance-code))
=> ("Object *temp_0 = funcall(-, x1, x2);"
    "Object *temp_1 = funcall(sq, temp_0);"
    "Object *temp_2 = funcall(-, y1, y2);"
    "Object *temp_3 = funcall(sq, temp_2);"
    "Object *temp_4 = funcall(+, temp_1, temp_3);"
    "Object *temp_5 = funcall(sqrt, temp_4);"
    "return temp_5;")
```

The next steps to take are to implement function lookup. Since `-` cannot be
used as a variable in C, the compiler must be able to lookup the handwritten C
function `la_diff`, which implements subtraction. The code should read
`funcall(la_diff, x1, x2)`.

Next, I want to implement the `if` special form, which should allow me to
compile most of `core.lisp` into C.

### August 27

I consider the interpreter be mostly feature complete. It can read a lisp file
from disk and interpret it. The most important special forms have been
implemented, as well as macros. I will definitely need string manipulation
for compiling to C, but they have yet to be written.

## Resources

- [Make a Lisp](https://github.com/kanaka/mal)
- [Lateral lisp](https://en.wikipedia.org/wiki/Lisp#Types)
- [Crafting Interpreters](https://craftinginterpreters.com/contents.html)

## License

This project is released under the GPL v3.0.
