CC=gcc
CFLAGS=-O3 -Wall -ansi -pandemic -lm

main: main.c
	${CC} ${CFLAGS} main.c -o main

debug: main.c
	${CC} ${CFLAGS} -DDEBUG=1 main.c -o main

clean:
	rm main
