CC = gcc
FLAGS = -Wall -Wextra -std=c99 -pedantic -ggdb
LIBS = -lreadline

all: lateral

clean:
	rm lateral *.o

lateral: lateral.o reader.o printer.o list.o object.o
	$(CC) $(FLAGS) lateral.o reader.o printer.o list.o object.o $(LIBS) -o lateral

lateral.o: lateral.c reader.h
	$(CC) $(FLAGS) lateral.c $(LIBS) -c

reader.o: reader.h reader.c
	$(CC) $(FLAGS) reader.c -c

printer.o: printer.h printer.c
	$(CC) $(FLAGS) printer.c -c

list.o: list.h list.c
	$(CC) $(FLAGS) list.c -c

object.o: object.h object.c
	$(CC) $(FLAGS) object.c -c
