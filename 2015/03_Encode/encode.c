#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// C Is fun!
char* encode(char* i) {
    char *o = (char*) malloc(sizeof(char) * 3);
    char *p = i, *q = i;
    char *s = o;

    do {
        if (*p != *q) {
            if (p - q > 3 || *q == '~') {
                s -= (p - q);
                while (q < p) {
                    int i = p - q >= 26 ? 26 : p - q;
                    *s++ = '~';
                    *s++ = 'A' + i - 1;
                    *s++ = *q;
                    q += i;
                }
            }
            q = p;
        }
        *s++ = *p;
    } while (*p++ != 0);
    *s = 0;

    return o;
}

char* decode (char* i) {
    char *o = (char*) malloc(sizeof(char) * 32);
    char *p = i;
    char *q = o;

    do {
        if (*p == '~') {
            int i = *++p - 'A' + 1;
            for (; i > 0; --i) {;
              *q++ = *(p + 1);
            }
            ++p;
        } else {
            *q++ = *p;
        }
    } while (*p++ != 0);

    *q = 0;
    return o;
}
