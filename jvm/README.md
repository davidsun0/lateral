# Lateral Lisp

Lateral Lisp is another Lisp dialect on the JVM (Java Virtual Machine).

## Goals
- Simple to use: there should be minimal setup and working with the JVM should be easy
- Strong JVM interop: make it easy to generate bytecode and custom classes

## Features
- Inline JVM bytecode for interop with other JVM languages
- Dynamic: anything can be redefined on the fly (coming soon), no forward declarations

## Dependencies
- [ASM](https://asm.ow2.io/index.html) for bytecode generation
