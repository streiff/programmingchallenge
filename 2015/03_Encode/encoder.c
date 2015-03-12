#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "encode.h"

int main(int argc, char** argv) {

    int inputsize = 4;
    char* input = (char*) malloc(inputsize);
    int p = 0;
    
    while (!feof(stdin)) {
        if (p + 1 >= inputsize) {
            inputsize *= 2;
            input = (char*) realloc(input, inputsize);
        }
        fgets(input + p, inputsize - p, stdin);
        p = strlen(input);
    }

    if (strstr(*argv, "encoder")) {
        printf("%s\n", encode(input));
    } else if (strstr(*argv, "decoder")) {
        printf("%s\n", decode(input));
    } else {
        printf("What you talking about Willis?\n");
    }

}
