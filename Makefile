CC = gcc
FLAGS = -Wall -Wextra -std=c99 -pedantic -ggdb
LIBS = -lreadline

all: lateral

lateral: lateral.o reader.o
	$(CC) $(FLAGS) lateral.o reader.o $(LIBS) -o lateral

lateral.o: lateral.c reader.h
	$(CC) $(FLAGS) lateral.c $(LIBS) -c

reader.o: reader.h reader.c
	$(CC) $(FLAGS) reader.c -c

clean:
	rm lateral *.o
