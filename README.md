# Lateral Lisp

Lateral is an interpreter for a basic dialect of Lisp.

This project is for understanding how Lisps work. As a result I'm using
as few dependencies as I can and writing most of the codebase from scratch.

Lateral was originally based off of [Make a Lisp](https://github.com/kanaka/mal).

## Features

- Precise Mark and Sweep garbage collection
- Tail call recursion
- Macros

## Building

Run the provide Makefile.

`make all`

### Dependencies

- GNU readline
