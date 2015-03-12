#include "player.h"
#include "data.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>

/* Just for Eric - he loves switches so why not nested conditionals too! */
#define DECODE_DIRECTION(X) (X == DIR_UP ? "up" : (X == DIR_DOWN ? "down" : (X == DIR_NORTH ? "north" : (X == DIR_SOUTH ? "south" : (X == DIR_EAST ? "east" : (X == DIR_WEST ? "west" : "void"))))))
#define SPACE " "

void lower(char*);

struct player* createplayer() {
    struct player* p = (struct player*) malloc(sizeof(struct player));
    p->room = 0;
    p->status = PLAYER_ALIVE;
    return p;
}

void destroyplayer(struct player* p) {
    free(p);
}

void player_look(struct player* p, int consocket, struct world* w) {
    int i;
    struct room* r = w->rooms[p->room];
    send(consocket, r->text, strlen(r->text), 0);

    if (r->numexits > 0) {
        char* exits = "\nExits are: ";
        send(consocket, exits, strlen(exits), 0);

        for (i = 0; i < r->numexits; ++i) {
            struct exit* e = r->exits[i];
            char* exittext = DECODE_DIRECTION(e->direction);
            send(consocket, exittext, strlen(exittext), 0);

            if (i != r->numexits - 1) {
                send(consocket, ", ", 2, 0);
            }
        }
        send(consocket, "\n\n", 2, 0);
    }
}

void player_prompt(struct player* p, int consocket) {
    char* prompt = "\n10hp 10mp 10st> ";
    send(consocket, prompt, strlen(prompt), 0);
}

int player_parse_direction(char* dir) {
    lower(dir);
     if (strcmp("up", dir) == 0 || strcmp("u", dir) == 0) {
         return DIR_UP;
     } else if (strcmp("down", dir) == 0 || strcmp("d", dir) == 0) {
         return DIR_DOWN;
     } else if (strcmp("north", dir) == 0 || strcmp("n", dir) == 0) {
         return DIR_NORTH;
     } else if (strcmp("south", dir) == 0 || strcmp("s", dir) == 0) {
         return DIR_SOUTH;
     } else if (strcmp("east", dir) == 0 || strcmp("e", dir) == 0) {
         return DIR_EAST;
     } else if (strcmp("west", dir) == 0 || strcmp("w", dir) == 0) {
         return DIR_WEST;
     } else {
         return DIR_VOID;
     }
}

void player_parse_look(struct player* p, int consocket, struct world* w) {
    char* tok = strtok(NULL, SPACE);

    if (tok == NULL) {
        player_look(p, consocket, w);
    } else {
        struct room* r = w->rooms[p->room];
        int dir = player_parse_direction(tok);
        int i;
        for (i = 0; i < r->numexits; ++i) {
            struct exit* e = r->exits[i];
            if (e->direction == dir) {
                send(consocket, e->text, strlen(e->text), 0);
                return;
            }
        }
        char* t = "There is nothing in that direction.";
        send(consocket, t, strlen(t), 0);
    }
}

void player_parse_move(struct player* p, int consocket, struct world* w, int dir) {
    char* tok = strtok(NULL, SPACE);

    if (tok == NULL) {
        struct room* r = w->rooms[p->room];

        int i;
        for (i = 0; i < r->numexits; ++i) {
            struct exit* e = r->exits[i];
            if (e->direction == dir) {
                p->room = e->toroom;
                player_look(p, consocket, w);

                if (!strcmp("DEATH", w->rooms[p->room]->type)) {
                    p->status = PLAYER_DEAD;
                }
                return;
            }
        }
        char* t = "You cannot go in that direction";
        send(consocket, t, strlen(t), 0);
    } else {
        char* t = "I get why you want to go there but you don't need to be so verbose about it.\nI'm staying right here until you can be more consise.\n";
        send(consocket, t, strlen(t), 0);
    }
}

void player_parse_cast(struct player* p, int consocket, struct world* w) {
    char* tok = strtok(NULL, SPACE);

    if (!tok) {
        char* t = "What do you want me to cast?\n";
        send(consocket, t, strlen(t), 0);
        return;
    }

    lower(tok);
    if (strcmp(tok, "encode") == 0) {
        char* t = encode(strtok(NULL, ""));
        send(consocket, t, strlen(t), 0);
        free(t);
    } else if (strcmp(tok, "decode") == 0) {
        char* t = decode(strtok(NULL, ""));
        send(consocket, t, strlen(t), 0);
        free(t);
    } else { 
        char* t = "Sorry - i don't know that spell\n";
        send(consocket, t, strlen(t), 0);
    }
}

int player_parse(struct player* p, int consocket, struct world* w, char* text) {
    char* tok = strtok(text, SPACE);
    lower(tok);
    if (tok == NULL) {
        return 1;
    } else if (strcmp(tok, "l") == 0 || 
               strcmp(tok, "look") == 0) {
        player_parse_look(p, consocket, w);
        return 1;
    } else if (strcmp(tok, "u") == 0 || 
               strcmp(tok, "up") == 0) {
        player_parse_move(p, consocket, w, DIR_UP);
        return 1;
    } else if (strcmp(tok, "d") == 0 || 
               strcmp(tok, "down") == 0) {
        player_parse_move(p, consocket, w, DIR_DOWN);
        return 1;
    } else if (strcmp(tok, "e") == 0 || 
               strcmp(tok, "east") == 0) {
        player_parse_move(p, consocket, w, DIR_EAST);
        return 1;
    } else if (strcmp(tok, "w") == 0 || 
               strcmp(tok, "west") == 0) {
        player_parse_move(p, consocket, w, DIR_WEST);
        return 1;
    } else if (strcmp(tok, "n") == 0 || 
               strcmp(tok, "north") == 0) {
        player_parse_move(p, consocket, w, DIR_NORTH);
        return 1;
    } else if (strcmp(tok, "s") == 0 || 
               strcmp(tok, "south") == 0) {
        player_parse_move(p, consocket, w, DIR_SOUTH);
        return 1;
    } else if (strcmp(tok, "cast") == 0) {
        player_parse_cast(p, consocket, w);
        return 1;
    } else if (strcmp(tok, "quit") == 0 || 
               strcmp(tok, "q") == 0 ||
               strcmp(tok, "exit") == 0) {
        return 0;
    } else {
        char* t = "Huh?\n";
        send(consocket, t, strlen(t), 0);
    }

    return 1;
}

void lower(char* p) { for(; *p; ++p) *p = tolower(*p); }
