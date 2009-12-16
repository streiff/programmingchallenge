#import "List.h"

@interface PermutationGenerator : Object {
    BOOL noMoreResults;
    int size;
    int* indexList;  
}

- (PermutationGenerator*) initWithSize: (int) size;
- (const int*) nextPermutation;
- free;

@end
