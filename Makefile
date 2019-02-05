CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -pedantic -ggdb
LDFLAGS = -lreadline

source = $(wildcard *.c)
objects = $(source:.c=.o)

all: lateral

.PHONY: clean
clean:
	rm lateral *.o

lateral: $(objects)
	$(CC) $(CFLAGS) -o lateral $^ $(LDFLAGS)
