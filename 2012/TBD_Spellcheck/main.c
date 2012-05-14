#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#define TRUE  1
#define FALSE 0

#define ARG_DICT_FILE 1
#define ARG_TEXT_FILE 2

#define NODE_SZ 27
#define MAX_FILE_SZ  10000000
#define MAX_ARRAY_SZ 20000000

#define DICT_CACHE "dict.bin"

int getChrIdx(char chr) {
    if (isalpha(chr)) {
        return tolower(chr) - 'a' + 1;
    } else {
        return -1;
    }
}

long* createLookupArray() {
    return (long*) calloc(MAX_ARRAY_SZ, sizeof(long));
}

long addWord(long* lookup, long next_free, char* start, char* end) {
    int curr_idx = 0;
    while (start <= end) {
        int chr_idx = getChrIdx(*start++);

        if (chr_idx < 0 || chr_idx > 26) {
            continue;
        }

        if (lookup[curr_idx + chr_idx] == 0) {
            lookup[curr_idx + chr_idx] = next_free;
            next_free += NODE_SZ;
        }
        curr_idx = lookup[curr_idx + chr_idx];
    }
    lookup[curr_idx] = 1;
    return next_free;
}

void createMMap(long* lookup) {
    int fd = open(DICT_CACHE, O_RDWR | O_CREAT | O_TRUNC, (mode_t) 0600);
    lseek(fd, MAX_ARRAY_SZ * sizeof(long) - 1, SEEK_SET);
    write(fd, "", 1);
    long* map = (long*) mmap(0, MAX_ARRAY_SZ * sizeof(long), PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

    for (int i = 0; i < MAX_ARRAY_SZ; ++i) {
        *map++ = *lookup++;
    }
    munmap(map, MAX_ARRAY_SZ * sizeof(long));
    close(fd);
}

long findWord(long* lookup, char* start, char* end) {
    int curr_position = 0;
    while (start <= end) {
        int chr_idx = getChrIdx(*start);

        if (chr_idx < 0) {
            return FALSE;
        }

        if (lookup[curr_position + chr_idx] == 0) {
            return FALSE;
        }

        curr_position = lookup[curr_position + chr_idx];
        ++start;
    }

    return lookup[curr_position];
}

void readDictFile(char *filename, long* lookup) {
    long next_free = NODE_SZ;
    long file_contents_sz;
    char *file_contents;
    FILE *dict_file;

    dict_file = fopen(filename, "r");
    file_contents = (char*) malloc (sizeof(char) * MAX_FILE_SZ);
    file_contents_sz = fread(file_contents, 1, MAX_FILE_SZ, dict_file);

    char* start_word = file_contents + 1;
    char* end_word;

    for (long i = 1; i < file_contents_sz; ++i) {
        char chr = file_contents[i];
        if (chr == '\n') {
            end_word = file_contents + i - 1;
            next_free = addWord(lookup, next_free, start_word, end_word);
            start_word = file_contents + i + 1;
        }
    }
}

int main(int argc, char **argv) {
    long* lookup;
    int fd = open(DICT_CACHE, O_RDONLY);
    if (fd == -1) {
        lookup = createLookupArray();
        readDictFile(argv[ARG_DICT_FILE], lookup);
        createMMap(lookup);
        fd = open(DICT_CACHE, O_RDONLY);
    }

    lookup = (long*) mmap(0, MAX_ARRAY_SZ * sizeof(long), PROT_READ, MAP_SHARED, fd, 0);

    long i = 0;
    FILE* text_file = fopen(argv[ARG_TEXT_FILE], "r");
    char* text_contents = (char*) malloc (sizeof(char) * MAX_FILE_SZ);
    long  text_contents_sz = fread(text_contents, 1, MAX_FILE_SZ, text_file);
    char* start_word = isspace(text_contents[0]) ?  text_contents + text_contents_sz : text_contents;
    char* end_word = NULL;

    for (; i < text_contents_sz; ++i) {
        if (!isalpha(text_contents[i])) {
            end_word = text_contents + i - 1;
            if (end_word > start_word && !findWord(lookup, start_word, end_word)) {
                while (start_word <= end_word) {
                    putc(*(start_word++), stdout);
                }
                putc('\n', stdout);
            }
            start_word = text_contents + i + 1;
        }
    }

    return 0;
}
