CFLAGS=-g -O -Wall -std=gnu99

all: clean
	gcc $(CFLAGS) -c String.m
	gcc $(CFLAGS) -c FileReader.m
	gcc $(CFLAGS) -c List.m
	gcc $(CFLAGS) -c Person.m
	gcc $(CFLAGS) -c main.m
	gcc $(CFLAGS) -lobjc main.o FileReader.o String.o List.o Person.o -o main

clean:
	rm -f main
	rm -f *.o
