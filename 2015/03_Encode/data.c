#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "data.h"

#define NEWLINE "\r\n"

int parsenumberrooms(char* roomdata);
struct room* parseroom();
void parseexits(struct room* roomdata);
void parseitems(struct room* roomdata);
void parsemobs(struct room* roomdata);

struct world* createworld(char* filename) {
    char* buffer = 0;
    long length;

    FILE* f = fopen(filename, "rb");
    if (f) {
        fseek(f, 0, SEEK_END);
        length = ftell(f);
        fseek(f, 0, SEEK_SET);
        buffer = (char*) malloc(length);
        fread(buffer, 1, length, f);
    }
    fclose(f);

    struct world* w = (struct world*) malloc(sizeof(struct world));

    w->numberrooms = parsenumberrooms(buffer);
    w->contents = buffer;

    w->rooms = (struct room**) malloc(sizeof(struct room*) * w->numberrooms);

    int i;
    for (i = 0; i < w->numberrooms; ++i) {
        w->rooms[i] = parseroom();
    }

    return w;
}

int parsenumberrooms(char* roomdata) {
    return atoi(strtok(roomdata, NEWLINE));
}

struct room* parseroom() {
    struct room* r = (struct room*) malloc(sizeof(struct room));

    r->id = atoi(strtok(NULL, NEWLINE));
    r->type = (char*) malloc(sizeof(char) * 16);
    strcpy(r->type, strtok(NULL, NEWLINE));

    int textlines = atoi(strtok(NULL, NEWLINE));

    r->text = (char*) malloc(sizeof(char) * 4000);
    int offset = 0;
    int i;
    for (i = 0; i < textlines; ++i) {
        char* line = strtok(NULL, NEWLINE);
        strcpy(r->text + offset, line);
        offset += strlen(line);
        r->text[offset] = '\n';
        offset++;
    }

    parseexits(r);
    parseitems(r);
    parsemobs(r);

    return r;
}

void parseexits(struct room* r) {
    r->numexits = atoi(strtok(NULL, NEWLINE));
    r->exits = (struct exit**) malloc(sizeof(struct exit*) * r->numexits);

    int i;
    for (i = 0; i < r->numexits; ++i) {
         r->exits[i] = (struct exit*) malloc(sizeof(struct exit));
         char* dir = strtok(NULL, NEWLINE);
         if (!strcmp("up", dir)) {
             r->exits[i]->direction = DIR_UP;
         } else if (!strcmp("down", dir)) {
             r->exits[i]->direction = DIR_DOWN;
         } else if (!strcmp("north", dir)) {
             r->exits[i]->direction = DIR_NORTH;
         } else if (!strcmp("south", dir)) {
             r->exits[i]->direction = DIR_SOUTH;
         } else if (!strcmp("east", dir)) {
             r->exits[i]->direction = DIR_EAST;
         } else if (!strcmp("west", dir)) {
             r->exits[i]->direction = DIR_WEST;
         } else {
             r->exits[i]->direction = DIR_VOID;
         }
         r->exits[i]->toroom = atoi(strtok(NULL, NEWLINE));

         char* line = strtok(NULL, NEWLINE);
         r->exits[i]->text = (char*) malloc(sizeof(char) * strlen(line) + 1);
         strcpy(r->exits[i]->text, line);
    }
}

void parseitems(struct room* r) {
    r->numitems = atoi(strtok(NULL, NEWLINE));
    r->items = (struct item**) malloc(sizeof(struct item*) * r->numitems);

    int i;
    for (i = 0; i < r->numitems; ++i) {
         r->items[i] = (struct item*) malloc(sizeof(struct item));

         char* line = strtok(NULL, NEWLINE);
         r->items[i]->keyword = (char*) malloc(sizeof(char) * strlen(line) + 1);
         strcpy(r->items[i]->keyword, line);

         line = strtok(NULL, NEWLINE);
         r->items[i]->text = (char*) malloc(sizeof(char) * strlen(line) + 1);
         strcpy(r->items[i]->text, line);
    }
}

void parsemobs(struct room* r) {
    r->nummobs = atoi(strtok(NULL, NEWLINE));
    r->mobs = (struct mob**) malloc(sizeof(struct mob*) * r->nummobs);

    int i;
    for (i = 0; i < r->nummobs; ++i) {
         r->mobs[i] = (struct mob*) malloc(sizeof(struct mob));

         char* line = strtok(NULL, NEWLINE);
         r->mobs[i]->keyword = (char*) malloc(sizeof(char) * strlen(line) + 1);
         strcpy(r->mobs[i]->keyword, line);

         line = strtok(NULL, NEWLINE);
         r->mobs[i]->text = (char*) malloc(sizeof(char) * strlen(line) + 1);
         strcpy(r->mobs[i]->text, line);
    }
}


void destroyworld(struct world* w) {
    int i, j;

    if (w->contents != NULL) free(w->contents);

    for (i = 0; i < w->numberrooms; ++i) {
        if (w->rooms[i]->type != NULL) free(w->rooms[i]->type);
        if (w->rooms[i]->text != NULL) free(w->rooms[i]->text);

        for (j = 0; j < w->rooms[i]->numexits; ++j) {
            if (w->rooms[i]->exits[j]->text != NULL) free(w->rooms[i]->exits[j]->text);
            free(w->rooms[i]->exits[j]);
        }
        free(w->rooms[i]->exits);

        for (j = 0; j < w->rooms[i]->numitems; ++j) {
            if (w->rooms[i]->items[j]->keyword != NULL) free(w->rooms[i]->items[j]->keyword);
            if (w->rooms[i]->items[j]->text != NULL) free(w->rooms[i]->items[j]->text);
            free(w->rooms[i]->items[j]);
        }
        free(w->rooms[i]->items);

        for (j = 0; j < w->rooms[i]->nummobs; ++j) {
            if (w->rooms[i]->mobs[j]->keyword != NULL) free(w->rooms[i]->mobs[j]->keyword);
            if (w->rooms[i]->mobs[j]->text != NULL) free(w->rooms[i]->mobs[j]->text);
            free(w->rooms[i]->mobs[j]);
        }
        free(w->rooms[i]->mobs);

        free(w->rooms);
    }

    free(w);
}
