#include "server.h"

struct connection* initconn(int port) {
    struct connection* conn;
  
    conn = (struct connection*) malloc(sizeof(struct connection));
    memset(conn, 0, sizeof(struct connection));

    conn->dest = (struct sockaddr_in*) malloc(sizeof(struct sockaddr_in));
    memset(conn->dest, 0, sizeof(struct sockaddr_in));

    conn->serv = (struct sockaddr_in*) malloc(sizeof(struct sockaddr_in));
    memset(conn->serv, 0, sizeof(struct sockaddr_in));
   
    conn->socksize = sizeof(struct sockaddr_in);
  
    conn->serv->sin_family = AF_INET;
    conn->serv->sin_addr.s_addr = htonl(INADDR_ANY);
    conn->serv->sin_port = htons(port);
    conn->socket = socket(AF_INET, SOCK_STREAM, 0);
  
    if (bind(conn->socket, (struct sockaddr*) conn->serv, sizeof(struct sockaddr))) {
        printf("Cannot bind to socket. Aborting...");
        free(conn->serv);
        free(conn->dest);
        free(conn);
        return NULL;
    }

    if (listen(conn->socket, 1)) {
        printf("Cannot listen to socket. Aborting...");
        free(conn->serv);
        free(conn->dest);
        free(conn);
        return NULL;
    }
  
    return conn;
}

int acceptconn(struct connection* conn) {
    return accept(conn->socket, (struct sockaddr *) conn->dest, &conn->socksize);
}


void destroyconn(struct connection* conn) {
    if (conn->dest != NULL) free(conn->dest);
    if (conn->serv != NULL) free(conn->serv);
    free(conn);
}
