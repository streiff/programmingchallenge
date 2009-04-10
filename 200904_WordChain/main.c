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
    char *value;
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
Link* readDictFile(char*, int);
Link* parseWords(char*, int, int);
Link* findWord(char*, char*, int, Link*);
int getDifference(char*, char*, int);
void removeLink(Link**, Link*);
Link* createLink(Link*, Link*, char*);

void addWord(char*);

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
    Link *word_list;
    int word_sz;


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

    word_sz = strlen(start_word);
    word_list = readDictFile(filename, word_sz);

    found_word = findWord(end_word, start_word, word_sz, word_list);
    
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
    printf("Word nodes created: %i\n", g_word_node_cnt);
    #endif

    return 0;
}

/* OTHER FUNCTIONS ===================================================== */
Link* readDictFile(char *filename, int word_sz) {
    long file_contents_sz;
    char *file_contents;
    FILE *dict_file;
    Link *words;

    #ifdef DEBUG
    size_t file_read_sz;
    #endif

    dict_file = fopen(filename, "r");

    #ifdef DEBUG
    if (dict_file == NULL) {
        printf("Error: Could not open file %s", filename);
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

    words = parseWords(file_contents, file_contents_sz, word_sz);

    #ifdef DEBUG
    free(file_contents);
    #endif

    return words;
}

Link* parseWords(char *s, int l, int ws) {
    int start_pos = 0;
    int newline_pos;
    int word_sz;
    char *w;

    Link *p, *q, *r = NULL;


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
        if (word_sz == ws) {
            r = malloc(sizeof(Link));
            #ifdef DEBUG
            if (r == NULL) {
                printf ("Error: could not allocate memory for real node.\n");
                exit(1);
            }
            #endif

            r->value = (char*) malloc(sizeof(char) * ws + 1);
            #ifdef DEBUG
            if (r->value == NULL) {
                printf ("Error: could not allocate memory for temp word\n");
                exit(1);
            }

            #endif
            strncpy(r->value, s + start_pos, word_sz);

            
            r->prev = q;
            q->next = r;
            q = r;

            w = (char*) malloc(sizeof(char) * ws + 1);
            strncpy(w, s + start_pos, word_sz);
            addWord(w);
        }

        start_pos = newline_pos + 1;
    }
    

    if (r != NULL) {
        r->next = NULL;
    }
    
    if (p == q) {
        printf ("Error: No words found of same length\n");
        exit(1);
    }
    q = p->next;
    q->prev = NULL;

    #ifdef DEBUG
    free(p);
    #endif
    return q;
}

Link* findWord(char *start_word, char *end_word, int word_sz, Link *words) {
    Link *root_node;
    Link *n;
    Link *o = NULL;
    Link *cn = NULL;    
    Link *p;
    char *curr_word;
    int t;
    int found_start = 0;
    int found_end = 0;
    int found_words;
    
    root_node = createLink(NULL, NULL, start_word);
    curr_word = start_word;
    
    /* build up the first set of words. special case since the start and
       end word will be in the list. We need to remove the start and end
       words -- erroring if they are not there.
    */
    for (p = words; p != NULL; p = p->next) {
        if (!found_start && strcmp(p->value, start_word) == 0) {
            ++found_start;
            removeLink(&words, p);
            continue;
        }
        
        t = getDifference(start_word, p->value, word_sz);
        if (!found_end && strcmp(p->value, end_word) == 0) {
            ++found_end;
            
            if (t == 1) {
                n = createLink(root_node, o, p->value);
                o = n;
                removeLink(&words, p);
                return n;
            }
            removeLink(&words, p);
        } else if (t == 1) {
            n = createLink(root_node, o, p->value);
            o = n;
            removeLink(&words, p);
        }
    }
    
    if (found_start == 0 || found_end == 0) {
        #ifdef DEBUG
        printf("Start word (%d) or end word (%d) not found\n", found_start, found_end);
        #endif
        return NULL;
    }
     
    if (o == NULL) {
        #ifdef DEBUG
        printWords(words);
        printf("o null with start word %s \n", start_word);
        #endif
        return NULL;
    }

    if (words == NULL) {
        #ifdef DEBUG
        printWords(words);
        printf("outta words afer first scan");
        #endif
        return NULL;
    }
    
    /* main loop. build up the layers till we have our word. */
    do {
        found_words = 0;

        cn = o;
        o = NULL;

        while (cn != NULL) {
            curr_word = cn->value;

            if (getDifference(curr_word, end_word, word_sz) == 1) {
                n = createLink(cn, o, end_word);
                o = n;
                return n;
            }

            for (p = words; p != NULL; p = p->next) {
                if (getDifference(curr_word, p->value, word_sz) == 1) {
                    ++found_words;
                    n = createLink(cn, o, p->value);
                    o = n;
                    removeLink(&words, p);
                }
            }
            cn = cn->next;

        }

    } while (found_words != 0);

    return NULL;
}

int getDifference(char *w1, char *w2, int word_sz) {
    int i;
    int d = 0;
    
    for (i = 0; i < word_sz && d <= 1; ++i) {
        if (w1[i] != w2[i]) {
            ++d;
        }
    }
    
    
    return d;
}

void removeLink(Link **start, Link *p) {
    if (p != *start) {
        p->prev->next = p->next;
        
        if (p->next != NULL) {
            p->next->prev = p->prev;
        }
    } else {
        if (p->next != NULL) {
            p->next->prev = NULL;
        }
        *start = p->next;
    }
    #ifdef DEBUG
    free(p);
    #endif
}

Link* createLink(Link *n1, Link *n2, char *s) {
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

#ifdef DEBUG
void printWords(Link *words) {
    Link *p;
    
    for (p = words; p != NULL; p = p->next) {
        printf("word: %s\n", p->value);
    }
}

void printLinks(Link *nodes) {
    Link *n;
    
    for (n = nodes; n != NULL; n = n->next) {
        printf("word: %s\n", n->value);
    }
}
#endif
