#ifndef RT_PLAYER
#define RT_PLAYER

#include "data.h"

#define PLAYER_ALIVE 1
#define PLAYER_DEAD 2

#define MAX_PLAYERS 1024

struct player {
    int room;
    int status;
    int fd;
    char* name;
};

struct player* createplayer(int df);
void destroyplayer(struct player*);

void player_look(struct player*, struct world*);
void player_prompt(struct player*);
int player_parse(struct player*, struct world*, char* text);

#endif
