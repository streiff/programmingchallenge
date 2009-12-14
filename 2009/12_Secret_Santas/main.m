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
        Person *person = [[Person alloc] initWithFirstName: [(String*) [data at: 2] deepCopy]
                                         lastName:  [(String*) [data at: 1] deepCopy]
                                         email:  [(String*) [data at: 0] deepCopy]];
        [list push: person];
    }

    [list each:&printLine1];
    [list free];

    return 0;
}

