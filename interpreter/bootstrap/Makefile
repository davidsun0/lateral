CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -pedantic -ggdb
LDFLAGS = -lreadline

source = $(wildcard *.c)
objects = $(source:.c=.o)

all: lateral

.PHONY: clean
clean:
	rm a.out *.o

.PHONY: test
test:
	../test/test.py ./a.out ../test/simple.lisp

lateral: $(objects)
	$(CC) $(CFLAGS) -o a.out $^ $(LDFLAGS)
