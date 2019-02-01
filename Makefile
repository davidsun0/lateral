CC=gcc
FLAGS=-Wall -Wextra -std=c99 -pedantic
LIBS=-lreadline

all: lateral

lateral: lateral.c
	$(CC) $(FLAGS) lateral.c $(LIBS) -o lateral

clean:
	rm lateral *.o
