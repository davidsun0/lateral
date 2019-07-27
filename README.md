# Lateral Lisp

Lateral is an interpreter for a basic dialect of Lisp.

The goal of this project is to write a self-hosting Lisp compiler. It will use
a C backend. I might compile to JVM Bytecode because it would be like writing
assembly, but cooler (portable, easier debugging, easier to learn).

## Current Status

### 7-27

I consider the interpreter be mostly feature complete. It can read a lisp file
from disk and interpret it. The most important special forms have been
implemented, as well as macros. I will definitely need string manipulation
for compiling to C, but they have yet to be written.

## Examples

```lisp
user> (print "Hello World!")
Hello World!
=> nil
```

### Math

```lisp
user> (+ 1 2 3 4)
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
