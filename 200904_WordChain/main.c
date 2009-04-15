#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define START_WORD 1
#define END_WORD   2
#define FILENAME   3
#define LTR_CNT 26
#define LARGEST_WORD 10

#define MAX_FILE_SZ 800000
#define NUM_RUNS 100
/* DATA STRUCTURES ===================================================== */
struct Link_S {
    struct Link_S *p;
    void *value;
};

struct Word_S {
    struct Word_S** children;
    char *value;
};

typedef struct Link_S Link;
typedef struct Word_S Word;

/* GLOBALS ============================================================= */
Word *g_words;
int g_word_sz;
int g_adj_i, g_adj_j, g_adj_k;
int *g_word_ind;

/* DECLARATIONS ======================================================== */
void readDictFile(char*);
void parseWords(char*, char*);
Link* findWord(char*, char*);
int getDifference(char*, char*);
Link* createLink(Link*, void*);
Word* createWord(char*);

Word* getAdjWord(char*, int);
void addWord(char*);
Word* removeWord(char*);

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
    int i;

    g_words = createWord(NULL);
    g_word_sz = strlen(start_word);
    g_word_ind = (int*) calloc(g_word_sz * 2, sizeof(int));

    readDictFile(filename);

    if (getDifference(start_word, end_word) == 0 && removeWord(start_word) != NULL) {
        printf(start_word);
        printf("\n");
        return 0;
    }

    found_word = findWord(end_word, start_word);

    if (found_word != NULL) {
        do {
	    *((char*) found_word->value + g_word_sz) = 0;
            printf(found_word->value);
            printf("\n");
            found_word = found_word->p;
        } while (found_word != NULL);
    } else {
        printf("No chain found\n");
    }
    return 0;
}

/* OTHER FUNCTIONS ===================================================== */
void readDictFile(char *filename) {
    long file_contents_sz;
    char *file_contents;
    FILE *dict_file;

    dict_file = fopen(filename, "r");
    file_contents = (char*) malloc (sizeof(char) * MAX_FILE_SZ);
    file_contents_sz = fread(file_contents, 1, MAX_FILE_SZ, dict_file);

    parseWords(file_contents, file_contents + file_contents_sz);
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

Link* findWord(char *start_word, char *end_word) {
    Link *curr_node;
    Link *l;
    Word *w;
    Link *w2;
    Link **stack;
    Link *stack_p;
    int stack_level = 0;
    int diff;
    int min_diff = 69;

    if (removeWord(start_word) == NULL) {
        printf("Start word not found.\n");
        exit(1);
    }

    if (removeWord(end_word) == NULL) {
        printf("End word not found.\n");
        exit(1);
    }
    
    curr_node = createLink(NULL, start_word);
    if (getDifference(start_word, end_word) == 1) {
        return createLink(curr_node, end_word);
    }

    stack = (Link**) calloc(LARGEST_WORD, sizeof(Link*));
    stack[0] = stack_p = createLink(NULL, curr_node);

    do {
        w2 = stack_p->value;
        w = getAdjWord(w2->value, 0);
	stack[stack_level] = stack_p->p;

        while (w != NULL) {
	    diff = getDifference(w->value, end_word);
            l = createLink(w2, w->value);

            if (diff == 1) {
                return createLink(l, end_word);
            }

	    stack[diff] = createLink(stack[diff], l);
	    if (diff < stack_level) {
		stack_level = diff;
	    }
	    
            w = getAdjWord(w2->value, 1);
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
    int i, j;
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
