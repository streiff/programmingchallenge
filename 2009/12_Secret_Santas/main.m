#import <stdio.h>

#import "FileReader.h"
#import "String.h"
#import "List.h"
#import "PermutationGenerator.h"

void printLine2(Object* obj) {
    printf("[%s]", (char*) [(String*) obj cStr]);
}

void printLine1(Object* obj) {
    [(List*) obj each: &printLine2];
    printf("\n");

}

int main(const int argc, const char *argv[]) {
    if (argc != 2) {
        printf("Must have a filename\n");
        return 1;
    }

    List *list = [[List alloc] init];
    String *str = [[String alloc] initWithCString: argv[1]];
    FileReader* read = [[FileReader alloc] initWithFilename: str];

    String *line;
    while ((line = [read readLine]) != nil) {
        List *data = [line split: ' '];
        [list push: data];
    }

    [list each:&printLine1];

    PermutationGenerator* gen = [[PermutationGenerator alloc] initWithSize: 4];
    printf("perms\n");
    int i, j;

    const int* perm;
    while ((perm  = [gen nextPermutation]) != NULL) {
        for (j = 0; j < 4; ++j) {
            printf(" %i", perm[j]);
        }
        printf("\n");
    }


    return 0;
}

