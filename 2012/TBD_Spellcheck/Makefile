CC=gcc
CFLAGS=-O3 -Wall -ansi -std=c99 -pedantic -lm 
DEBUG_FLAGS=-std=c99 -g -pg

main: main.c
	${CC} ${CFLAGS} main.c -o main

debug: main.c
	${CC} ${DEBUG_FLAGS} -DDEBUG=1 main.c -o main

clean:
	rm -f main dict.bin
