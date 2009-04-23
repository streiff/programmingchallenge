CC=gcc
CFLAGS=-O3 -Wall -ansi -pedantic -lm 
DEBUG_FLAGS=-g -pg

main: main.c
	${CC} ${CFLAGS} main.c -o main

debug: main.c
	${CC} ${DEBUG_FLAGS} -DDEBUG=1 main.c -o main

clean:
	rm main
