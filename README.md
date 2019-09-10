# Lateral Lisp

Lateral is an interpreter for a basic dialect of Lisp.

The goal of this project is to write a self-hosting Lisp compiler. It will use
a C backend. JVM Bytecode is also a planned compilation target.

## Current Status

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
    "return temp_5")
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

## Resources

- [Make a Lisp](https://github.com/kanaka/mal)
- [Lateral lisp](https://en.wikipedia.org/wiki/Lisp#Types)
- [Crafting Interpreters](https://craftinginterpreters.com/contents.html)

## License

This project is released under the GPL v3.0.
