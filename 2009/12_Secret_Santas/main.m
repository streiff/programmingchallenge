#import <stdio.h>

#import "FileReader.h" 
#import "List.h"
#import "Person.h"
#import "PermutationGenerator.h"
#import "String.h"


void printLine1(Object* obj) {
    printf("%s \n", [[(Person*) obj toString] cStr]);
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
        if ([data length] != 3) {
            printf("Error: Data file invalid. Line %s\n", [line cStr]);
            return -1;
        }
        Person *person = [[Person alloc] initWithFirstName: [(String*) [data at: 0] deepCopy]
                                         lastName:  [(String*) [data at: 1] deepCopy]
                                         email:  [(String*) [data at: 2] deepCopy]];
        [list push: person];
    }

    PermutationGenerator* p = [[PermutationGenerator alloc] initWithSize: [list length]];
    const int* perm;
    BOOL foundSolution = NO;
int j = 0;
    while (!foundSolution && (perm = [p nextPermutation]) != NULL) {
    ++j;
        int i;
        for (i = 0; i < [list length]; ++i) {
            Person* p1 = (Person*) [list at: i];
            Person* p2 = (Person*) [list at: perm[i]];

            if ([p1 isSameFamily: p2]) {
                break;
            }
        }
        foundSolution = i == [list length];
    }

    if (foundSolution) {
        int i;
        for (i = 0; i < [list length]; ++i) {
            Person* p1 = (Person*) [list at: i];
            Person* p2 = (Person*) [list at: perm[i]];
            printf("%s gives to %s\n", [[p1 toString] cStr], [[p2 toString] cStr]);
        }
    } else {
        printf("A list could not be generated.\n");
    }
    [list free];


    return foundSolution ? 0 : 1;
}

