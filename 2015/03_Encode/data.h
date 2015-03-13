#ifndef RT_DATA
#define RT_DATA

#define DIR_UP    0
#define DIR_DOWN  1
#define DIR_NORTH 2
#define DIR_SOUTH 3
#define DIR_EAST  4
#define DIR_WEST  5
#define DIR_VOID -1 

struct exit {
    int toroom;
    int direction;
    char* text;
};

struct item {
    char* keyword;
    char* text;
};

struct mob {
    char* keyword;
    char* text;
};

struct room {
    int id;
    char* type;
    char* text;

    int numexits;
    struct exit** exits;

    int numitems;
    struct item** items;

    int nummobs;
    struct mob** mobs;
};

struct world {
    int numberrooms;
    char* contents;
    struct room** rooms;
};


struct world* createworld(char* filename);
void destroyworld(struct world* w);

#endif

