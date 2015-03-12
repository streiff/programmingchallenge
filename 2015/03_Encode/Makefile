CC = gcc
PORTNUM?=2400
CFLAGS=-c -Wall -DPORTNUM=$(PORTNUM)
LDFLAGS=
SOURCES=main.c server.c data.c player.c encode.c
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=server

all: $(SOURCES) $(EXECUTABLE) encoder decoder

clean:
	rm -f *.o $(EXECUTABLE) encoder decoder

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

encoder: encoder.c
	$(CC) $(LDFLAGS) encode.o encoder.c -o $@

decoder: encoder.c
	$(CC) $(LDFLAGS) encode.o encoder.c -o $@
.c.o:
	$(CC) $(CFLAGS) $< -o $@


