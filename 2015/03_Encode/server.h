#ifndef RT_SERVER
#define RT_SERVER

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>

struct connection {
    struct sockaddr_in* dest;
    struct sockaddr_in* serv;

    int socket;
    socklen_t socksize;
};

struct connection* initconn(int port);

int acceptconn(struct connection* conn);

void destroyconn(struct connection* conn);

char* readconn(int ds);

#endif
