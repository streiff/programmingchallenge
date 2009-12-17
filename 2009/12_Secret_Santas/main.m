#import <time.h>
#import <stdio.h>
#import <stdlib.h>

#import "FileReader.h" 
#import "List.h"
#import "Person.h"
#import "PermutationGenerator.h"
#import "String.h"

void readFileIntoLists(String*, List*, List*);
void matchSantas(List*, List*);

int main(const int argc, const char *argv[]) {
    if (argc != 2) {
        printf("Must have a filename\n");
        return 1;
    }
    srandom(time(NULL));

    String *filename = [[String alloc] initWithCString: argv[1]];
    List *srcList = [[List alloc] init];
    List *destList = [[List alloc] init];

    readFileIntoLists(filename, srcList, destList);
    matchSantas(srcList, destList);

    for (int i = 0, j = [srcList length]; i < j; ++i) {
        printf("%s gives to %s\n", [[(Person*) [srcList at: i] toString] cStr],
                                   [[(Person*) [destList at: i] toString] cStr]);
    }

    [destList free];
    [srcList free];
    [filename free];

    return 0;
}

void readFileIntoLists(String *filename, List *l1, List *l2) {
    FileReader *read = [[FileReader alloc] initWithFilename: filename];
    String *line = nil;

    while ((line = [read readLine]) != nil) {
        List *data = [line split: ' '];
        if ([data length] == 3) {
            Person *person = [[Person alloc] initWithFirstName: [(String*) [data at: 0] deepCopy]
                                             lastName:  [(String*) [data at: 1] deepCopy]
                                             email:  [(String*) [data at: 2] deepCopy]];

            [l1 push: [person deepCopy]];
            [l2 push: [person deepCopy]];
            [person free];
        } else {
            printf("Error: Skipping invalid line: %s\n", [line cStr]);
        }
        [data free];
        [line free];
    }
    [read free];
}

void matchSantas(List* srcList, List* destList) {
    [destList shuffle];

    for (int i = 0, j = [srcList length]; i < j; ++i) {
        Person *p1 = (Person*) [srcList at: i];
        Person *p2 = (Person*) [destList at: i];

        if ([p1 isSameFamily: p2]) {
            [destList swap: i with: random() % j];
            i = 0;
        }
    }
}
