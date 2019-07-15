# Lateral Lisp

Lateral is an interpreter for a basic dialect of Lisp.

The Lateral interpreter is a personal project to learn about how interpreters
work. The current goal is to be able to self-host the language.

After that, I may add scripting features or video rendering features.
- Bash is awkward for certain file manipulations. Map and filter are common
actions I need and are easier to remember and write than Bash for loops.
- I want a scripting language to render short video clips (several seconds).
I'll be able to easily interface with filters using C and compose movement
using Lateral Lisp.

## Current Status

- Trying to find bugs in my garbage collector
- Rewriting eval to use a new program stack instead of C's call stack
- Developing errors and an error handling system
- Have an internal C hashmap structure, but no hashmap in lisp yet.

- Some basic arithmetic implemented
- Some list manipulation functions implemented (first, rest, cons, concat, list)
- Macros and lambdas are implemented, but quasiquote is not

## Examples

```lisp
user> (print "Hello World!")
Hello World!
=> nil
```

Math

```lisp
user> (+ 1 2 3 4)
=> 10
```

## Features

- Macros
- Mark and sweep garbage collection
- Tail call recursion (planned)
- Unicode support (planned)

## Building

Run the provide Makefile.

`make all`

### Dependencies

- GNU readline

## Resources

- [Make a Lisp](https://github.com/kanaka/mal)
- [Lateral lisp](https://en.wikipedia.org/wiki/Lisp#Types)
