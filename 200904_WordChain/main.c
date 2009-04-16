#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define START_WORD 1
#define END_WORD   2
#define FILENAME   3
#define LTR_CNT 26
#define LARGEST_WORD 10

#define NO_CHAIN_FOUND "No chain found\n"

#define RESET 0
#define RESUME 1

#define MAX_FILE_SZ 800000
#define NUM_RUNS 100
/* DATA STRUCTURES ===================================================== */
struct Link_S {
    struct Link_S *p;
    void *value;
};

struct Word_S {
    struct Word_S **children;
    char *value;
};

typedef struct Link_S Link;
typedef struct Word_S Word;

/* GLOBALS ============================================================= */
Word *g_words;
int g_word_sz;
int g_adj_i, g_adj_j, g_adj_k; /* adj word position */

/* DECLARATIONS ======================================================== */
int readDictFile(char*,char*, char*);

/* Parsing functions */
void parseWords(char*, char*);
int containsStartWord(char*, char*, char*);
int containsStartWordEndWord(char*, char*, char*, char*);

/* Word finders */
Link* findWord(char*, char*);
int getDifference(char*, char*);

/* DS Management */
Link* createLink(Link*, void*);
Word* createWord(char*);
void addWord(char*);
Word* removeWord(char*);
Word* getAdjWord(char*, int);

/* MAIN ================================================================ */
#ifdef DEBUG
int _main(int argc, char **argv);

int main(int argc, char **argv) {
    int i;

    for (i = 0; i < NUM_RUNS; ++i) {
        _main(argc, argv);
    }
}

int _main(int argc, char **argv) {
#else
int main(int argc, char **argv) {
#endif

    char *start_word = argv[START_WORD];
    char *end_word = argv[END_WORD];
    char *filename = argv[FILENAME];
    Link *found_word;

    g_words = createWord(NULL);
    g_word_sz = strlen(start_word);

    if (readDictFile(filename, start_word, end_word)) {
        /* we have found the chain already. */
        return 0;
    }

    found_word = findWord(start_word, end_word);
    if (found_word != NULL) {
        do {
            *((char*) found_word->value + g_word_sz) = 0;
            printf(found_word->value);
            printf("\n");
            found_word = found_word->p;
        } while (found_word != NULL);
    } else {
        printf(NO_CHAIN_FOUND);
    }
    return 0;
}

/* OTHER FUNCTIONS ===================================================== */
int readDictFile(char *filename, char *start_word, char *end_word) {
    long file_contents_sz;
    char *file_contents;
    FILE *dict_file;
    int diff;

    dict_file = fopen(filename, "r");
    file_contents = (char*) malloc (sizeof(char) * MAX_FILE_SZ);
    file_contents_sz = fread(file_contents, 1, MAX_FILE_SZ, dict_file);

    diff = getDifference(start_word, end_word);
    if (diff == 0) {
        if (containsStartWord(file_contents, 
                file_contents + file_contents_sz, start_word)) {
            printf(start_word);
            printf("\n");
            return 1;
        } else {
            printf(NO_CHAIN_FOUND);
            exit(1);
        }
    } else if (diff == 1) {
        if (containsStartWordEndWord(file_contents, 
                file_contents + file_contents_sz, start_word, end_word)) {
            printf(start_word);
            printf("\n");
            printf(end_word);
            printf("\n");
            return 1;
        } else {
            printf(NO_CHAIN_FOUND);
            exit(1);
        }
    } else {
        parseWords(file_contents, file_contents + file_contents_sz);
    }
    return 0;
}

void parseWords(char *start, char *end) {
    char *newline_pos = start;

    while (start < end) {
        while (*newline_pos != '\n' && ++newline_pos <= end);

        if (newline_pos - start == g_word_sz) {
            addWord(start);
        }

        start = ++newline_pos;
    }
}

int containsStartWord(char *start, char *end, 
        char *start_word) {
    char *newline_pos = start;

    while (start < end) {
        while (*newline_pos != '\n' && ++newline_pos <= end);

        if (newline_pos - start == g_word_sz && 
                getDifference(start_word, start) == 0) {
            return 1;
        }

        start = ++newline_pos;
    }
    return 0;
}

int containsStartWordEndWord(char *start, char *end, 
        char *start_word, char *end_word) {
    char *newline_pos = start;
    int start_found = 0;
    int end_found = 0;

    while (start < end) {
        while (*newline_pos != '\n' && ++newline_pos <= end);

        if (newline_pos - start == g_word_sz) {
            if (!start_found && getDifference(start_word, start) == 0) {
                if (end_found) {
                    return 1;
                }
                ++start_found;
            } else if (!end_found && getDifference(end_word, start) == 0) {
                if (start_found) {
                    return 1;
                }
                ++end_found;
            }
        }
        start = ++newline_pos;
    }
    return 0;
}

Link* findWord(char *start_word, char *end_word) {
    Link *curr_node;
    Link *l;
    Word *adj_word;
    Link *stack_val;
    Link **stack;
    Link *stack_p;
    int stack_level = g_word_sz;
    int diff;

    if (removeWord(start_word) == NULL) {
        printf("Start word (%s) not found.\n", start_word);
        exit(1);
    }

    if (removeWord(end_word) == NULL) {
        printf("End word (%s) not found.\n", end_word);
        exit(1);
    }
    
    curr_node = createLink(NULL, end_word);
    if (getDifference(start_word, end_word) == 1) {
        return createLink(curr_node, end_word);
    }

    stack = (Link**) calloc(LARGEST_WORD, sizeof(Link*));
    stack[0] = stack_p = createLink(NULL, curr_node);

    do {
        stack_val = stack_p->value;
        adj_word = getAdjWord(stack_val->value, RESET);
        stack[stack_level] = stack_p->p;

        while (adj_word != NULL) {
            diff = getDifference(adj_word->value, start_word);
            l = createLink(stack_val, adj_word->value);

            if (diff == 1) {
                return createLink(l, start_word);
            }

            stack[diff] = createLink(stack[diff], l);
            if (diff < stack_level) {
                stack_level = diff;
            }
            adj_word = getAdjWord(stack_val->value, RESUME);
        }

        do {
            stack_p = stack[stack_level];
        } while (stack_p == NULL && stack_level++ < LARGEST_WORD);
    } while (stack_p != NULL);

    return NULL;
}

int getDifference(char *w1, char *w2) {
    int i, d = 0;

    for (i = 0; i < g_word_sz; ++i) {
        if (*w1++ != *w2++) {
            ++d;
        }
    }

    return d;
}

Link* createLink(Link *p, void *v) {
    Link *n;
    n = (Link*) malloc(sizeof(Link));
    n->p = p;
    n->value = v;

    return n;
}

Word* createWord(char *c) {
    Word *w;
    w = (Word*) malloc(sizeof(Word));
    w->children = calloc(LTR_CNT, sizeof(Word*));
    return w;
}

void addWord(char* word) {
    int i;
    Word* p = NULL;
    Word* q = NULL;
    int c;

    i = 0;
    p = g_words;

    for (; i < g_word_sz; ++i) {
        c = word[i] - 'a';
        
        if (p->children[c] == NULL) {
            q = createWord(NULL);
            p->children[c] = q;
            p = q;
        } else {
            p = p->children[c];
        }
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
    return q;
}

Word* getAdjWord(char *word, int resume) {
    Word *p;

    if (resume) {
        goto ADJ_RESUME;
    }

    for (g_adj_i = 0; g_adj_i < g_word_sz; ++g_adj_i) {
        for (g_adj_j = 0; g_adj_j < LTR_CNT; ++g_adj_j) {
            p = g_words;
            for (g_adj_k = 0; g_adj_k < g_word_sz; ++g_adj_k) {
                if (g_adj_i != g_adj_k) {
                    p = p->children[word[g_adj_k] - 'a'];
                } else {
                    p = p->children[g_adj_j];
                }

                if (p == NULL) {
                    break;
                }
            }


            if (p != NULL) {
                return removeWord(p->value);
ADJ_RESUME:
            ;
            }
        }
    }

    return NULL;
}
