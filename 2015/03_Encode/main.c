#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <errno.h>
#include <unistd.h>
#include <netdb.h>

#include "server.h"
#include "data.h"
#include "player.h"
 
#ifndef PORTNUM
#define PORTNUM 2400
#endif

struct player** players;

int findplayeridx(struct player** players, int fd) {
    int i;
    for (i = 0; i < MAX_PLAYERS; ++i) {
        if (fd == 0 && players[i] == 0) {
            return i;
        } else if (players[i] != NULL && players[i]->fd == fd) {
            return i;
        }
    }
    return -1;
}
 
int main(int argc, char **argv)
{
    printf("Starting game on port %i\n", PORTNUM);
    printf("telnet to port %i to play or Ctrl-C to stop\n", PORTNUM);

    struct connection* conn = initconn(PORTNUM);
    if (conn == NULL) {
        printf("Try waiting for a few seconds for the port to become available again.\n");
        return -1;
    }

    struct world* w = createworld("world.dat");
    fd_set activefds, readfds;
    FD_ZERO(&activefds);
    FD_SET(conn->socket, &activefds);

    
    players = (struct player**) malloc(sizeof (struct player*) * MAX_PLAYERS);
    memset(players, 0, sizeof (struct player*) * MAX_PLAYERS);

    while(1) {
        int i;

        readfds = activefds;
        if (select (FD_SETSIZE, &readfds, NULL, NULL, NULL) < 0) {
            printf("Error selecting...\n");
            exit(EXIT_FAILURE);
        }

        for (i = 0; i < FD_SETSIZE; ++i) {
            if (FD_ISSET(i, &readfds)) {
                if (i == conn->socket) {
                    int consocket = acceptconn(conn);
                    FD_SET(consocket, &activefds);
                    printf("Incoming connection from %s\n", inet_ntoa(conn->dest->sin_addr));
                    struct player* p = createplayer(consocket);
                    strcpy(p->name, inet_ntoa(conn->dest->sin_addr));

                    int j = findplayeridx(players, 0);
                    players[j] = p;
                    player_look(p, w);
                    player_prompt(p);

                } else {
                    char* buff = readconn(i);

                    int keepplaying = 0;
                    int stayinalive = 1;
                    if (buff != NULL) {
                        int j = findplayeridx(players, i);
                        keepplaying = player_parse(players[j], w, buff);
                        stayinalive = players[j]->status != PLAYER_DEAD;

                        if (stayinalive) {
                            player_prompt(players[j]);
                        } else {
                            char* t = "You are dead.\n";
                            send(players[j]->fd, t, strlen(t), 0);
                        }
                    }

                    if (buff == NULL || !keepplaying || !stayinalive) {
                        printf("Connection closed...\n");
                        int j = findplayeridx(players, i);
                        free(players[j]);
                        players[j] = 0;

                        close(i);
                        FD_CLR(i, &activefds);

                    } else {
                        free(buff);
                    }
                }
            }
        }
    }
 
    destroyworld(w);
    close(conn->socket);
    destroyconn(conn);
    return 0;
}
