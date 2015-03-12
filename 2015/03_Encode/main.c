#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>

#include "server.h"
#include "data.h"
#include "player.h"
 
#ifndef PORTNUM
#define PORTNUM 2400
#endif
 
int main(int argc, char **argv)
{
    printf("Starting game on port %i\n", PORTNUM);
    printf("telnet to port %i to play or Ctrl-C to stop\n", PORTNUM);

    struct connection* conn = initconn(PORTNUM);
    if (conn == NULL) {
        printf("Try waiting for a few seconds for the port to become available again.\n");
        return -1;
    }
    int consocket = acceptconn(conn);
    struct world* w = createworld("world.dat");
 
    while(consocket) {
        printf("Incoming connection from %s\n", inet_ntoa(conn->dest->sin_addr));

        struct player* p = createplayer();

        player_look(p, consocket, w);
        
        int read;
        char* buff = (char*) malloc(sizeof(char) * 256);

        do {
            player_prompt(p, consocket);
            read = recv(consocket, buff, 255, 0);
            if (read > 2) buff[read-2] = 0;

        } while (read > 0 && player_parse(p, consocket, w, buff) && p->status != PLAYER_DEAD);

        if (p->status == PLAYER_DEAD) {
            char* t = "\nYou are dead.\n";
            send(consocket, t, strlen(t), 0);
        }

        free(buff);
        destroyplayer(p);

        close(consocket);
        printf("Connection closed. Waiting for next player.\n");

        consocket = acceptconn(conn);
    }
 
    destroyworld(w);
    close(conn->socket);
    destroyconn(conn);
    return 0;
}
