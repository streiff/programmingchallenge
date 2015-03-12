#ifndef RT_PLAYER
#define RT_PLAYER

#include "data.h"

#define PLAYER_ALIVE 1
#define PLAYER_DEAD 2

struct player {
    int room;
    int status;
};

struct player* createplayer();
void destroyplayer(struct player*);

void player_look(struct player*, int consocket, struct world*);
void player_prompt(struct player*, int consocket);
int player_parse(struct player*, int consocket, struct world*, char* text);

#endif
