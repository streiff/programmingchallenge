#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define START_WORD 1
#define END_WORD   2
#define FILENAME   3
#define LTR_CNT 26

/* DATA STRUCTURES ===================================================== */
struct Link_S {
    struct Link_S *prev;
    struct Link_S *next;
    void *value;
};

struct Word_S {
    struct Word_S **children;
    int num_children;
    char *value;
};

typedef struct Link_S Link;
typedef struct Word_S Word;

/* GLOBALS ============================================================= */
Word *g_words;
int g_word_sz;

#ifdef DEBUG
int g_word_node_cnt = 0;
#endif

/* DECLARATIONS ======================================================== */
void readDictFile(char*);
void parseWords(char*, int);
Link* findWord(char*, char*);
int getDifference(char*, char*);
Link* createLink(Link*, Link*, void*);

Word* getAdjWord(char*);
void addWord(char*);
Word* removeWord(char*);
Link* addToStack(Link*, Link*);

#ifdef DEBUG
void printWords(Link*);
void printLinks(Link*);
#endif

/* MAIN ================================================================ */
int main(int argc, char **argv) {
    char *start_word = argv[START_WORD];
    char *end_word = argv[END_WORD];
    char *filename = argv[FILENAME];
    Link *found_word;

    #ifdef DEBUG
    if (argc < 3) {
        printf("Error: not enough args\n");
        exit(1);
    }
    printf("start word: %s\n", start_word);
    printf("end word: %s\n", end_word);
    printf("size: %i\n", (int) strlen(start_word));
    #endif

    g_words = (Word*) malloc(sizeof(Word));
    g_words->children = (Word**) calloc(LTR_CNT, sizeof(Word*));
    g_words->num_children = 0;
    g_words->value = NULL;
    g_word_sz = strlen(start_word);

    readDictFile(filename);

    #ifdef DEBUG
    printf("Word nodes created: %i\n", g_word_node_cnt);
    #endif

    found_word = findWord(end_word, start_word);
    
    if (found_word != NULL) {
        do {
            printf(found_word->value);
            printf("\n");
            found_word = found_word->prev;
        } while (found_word != NULL);
    } else {
        printf("No chain found\n");
    }

    #ifdef DEBUG
    printf("Word nodes left: %i\n", g_word_node_cnt);
    #endif

    return 0;
}

/* OTHER FUNCTIONS ===================================================== */
void readDictFile(char *filename) {
    long file_contents_sz;
    char *file_contents;
    FILE *dict_file;

    #ifdef DEBUG
    size_t file_read_sz;
    #endif

    dict_file = fopen(filename, "r");

    #ifdef DEBUG
    if (dict_file == NULL) {
        printf("Error: Could not open file %s\n", filename);
        exit(1);
    }
    #endif

    fseek(dict_file, 0, SEEK_END);
    file_contents_sz = ftell(dict_file);
    rewind (dict_file);

    file_contents = (char*) malloc (sizeof(char) * file_contents_sz);

    #ifdef DEBUG
    if (file_contents == NULL) {
        printf ("Error: could not allocate memory for file contents.\n");
        exit(1);
    }

    file_read_sz =
    #endif

    fread(file_contents, 1, file_contents_sz, dict_file);

    #ifdef DEBUG
    if (file_contents_sz != file_read_sz) {
        printf ("Error: could not read file.\n");
        exit(1);
    }
    #endif

    fclose(dict_file);

    parseWords(file_contents, file_contents_sz);
}

void parseWords(char *s, int l) {
    int start_pos = 0;
    int newline_pos;
    int word_sz;
    char *w;
    Link *p, *q = NULL;


    p = malloc(sizeof(Link));
    #ifdef DEBUG
    if (p == NULL) {
        printf ("Error: could not allocate memory for dummy node.\n");
        exit(1);
    }
    #endif
    q = p;

    while (start_pos < l) {
        for (newline_pos = start_pos; s[newline_pos] != '\n' && newline_pos < l; ++newline_pos);

        word_sz = newline_pos - start_pos;
        if (word_sz == g_word_sz) {
            w = (char*) malloc(sizeof(char) * g_word_sz + 1);
            strncpy(w, s + start_pos, g_word_sz);
            addWord(w);
        }

        start_pos = newline_pos + 1;
    }
}

Link* findWord(char *start_word, char *end_word) {
    Link *curr_node;
    Link *l;
    Word *w;
    Link *w2;
    Link *stack;
    Link *end_stack;

    if (removeWord(start_word) == NULL) {
        printf("Start word not found.\n");
        exit(1);
    }

    if (removeWord(end_word) == NULL) {
        printf("End word not found.\n");
        exit(1);
    }
    
    curr_node = createLink(NULL, NULL, start_word);
    if (getDifference(start_word, end_word) == 1) {
        return createLink(curr_node, NULL, end_word);
    }

    stack = end_stack = createLink(NULL, NULL, curr_node);

    do {
        w2 = (Link*) stack->value;
        w = getAdjWord(w2->value);

        while (w != NULL) {
            l = createLink(stack->value, NULL, w->value);
            end_stack = addToStack(end_stack, l);

            if (getDifference(w->value, end_word) == 1) {
                return createLink(l, NULL, end_word);
            }
            w = getAdjWord(w2->value);
        }

        stack = stack->next;
    } while (stack != NULL);

    return NULL;
}

int getDifference(char *w1, char *w2) {
    int i;
    int d = 0;
    
    for (i = 0; i < g_word_sz && d <= 1; ++i) {
        if (w1[i] != w2[i]) {
            ++d;
        }
    }
    return d;
}

Link* createLink(Link *n1, Link *n2, void *s) {
    Link *n;
    n = (Link*) malloc(sizeof(Link));
    #ifdef DEGUG
    if (n == NULL) {
        printf("cannot allocate memory for node.");
        exit(1);
    }
    #endif
    n->prev = n1;
    n->next = n2;
    n->value = s;

    return n;
}

Link* addToStack(Link *end, Link *value) {
    Link *p;

    p = createLink(NULL, NULL, value);
    end->next = p;
    return p;
}

void addWord(char* word) {
    int i;
    Word* p;
    Word* q;
    int* word_ch;

    word_ch = malloc(sizeof(char) * g_word_sz);

    /* sanitize the words */
    for (i = 0; i < g_word_sz; ++i) {
        word_ch[i] = tolower(word[i]) - 'a';
        if (word_ch[i] < 0 || word_ch[i] >= 26) {
            return;
        }
    }

    /* add to graph */
    p = g_words;
    for (i = 0; i < g_word_sz; ++i) {
        #ifdef DEBUG
        ++g_word_node_cnt;
        #endif

        if (p->children[word_ch[i]] == NULL) {
            q = (Word*) malloc(sizeof(Word));
            q->children = (Word**) calloc(LTR_CNT, sizeof(Word*));
            q->num_children = 0;
            q->value = NULL;

            p->children[word_ch[i]] = q;
            p->num_children++;
        }
        p = p->children[word_ch[i]];
    }

    p->value = word;
}

Word* removeWord(char* w) {
    int i;
    Word *p;
    Word *q;

    p = q = g_words;
    for (i = 0; i < g_word_sz; ++i) {
        p = q;
        q = q->children[w[i] - 'a'];
        if (q == NULL) {
            return NULL;
        }
    }

    p->children[w[g_word_sz - 1] - 'a'] = NULL;
    p->num_children--;

    #ifdef DEBUG
    --g_word_node_cnt;
    printf("removing word: %s\n", q->value);
    #endif

    return q;
}

Word* getAdjWord(char *word) {
    Word *p;
    int i, j, k;

    for (i = 0; i < g_word_sz; ++i) {
        for (j = 0; j < LTR_CNT; ++j) {
            p = g_words;
            for (k = 0; k < g_word_sz; ++k) {
                if (i != k) {
                    p = p->children[word[k] - 'a'];
                } else {
                    p = p->children[j];
                }

                if (p == NULL) {
                    break;
                }
            }


            if (p != NULL) {
                return removeWord(p->value);
            }
        }
    }

    return NULL;
}

#ifdef DEBUG
void printWords(Link *words) {
    Link *p;
    
    for (p = words; p != NULL; p = p->next) {
        printf("word: %s\n", (char*) p->value);
    }
}

void printLinks(Link *nodes) {
    Link *n;
    
    for (n = nodes; n != NULL; n = n->next) {
        printf("word: %s\n", (char*) n->value);
    }
}
#endif
