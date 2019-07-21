# Lateral Lisp

Lateral is an interpreter for a basic dialect of Lisp.

I am in the process of rewriting the interpreter. I am much better at C after
writing the first interpreter and am making big structural changes in the
second revision. Old project is in src/ while the rewrite is in new/.

My goal with this interpreter is to write an interpreter / compiler in Lateral
Lisp. The plan is to perform syntax parsing in Lisp and use C as a backend.

## Current Status

- Rewriting eval to use a new program stack instead of C's call stack

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

## Building

Run the provide Makefile.

`make all`

### Dependencies

- GNU readline

## Resources

- [Make a Lisp](https://github.com/kanaka/mal)
- [Lateral lisp](https://en.wikipedia.org/wiki/Lisp#Types)
