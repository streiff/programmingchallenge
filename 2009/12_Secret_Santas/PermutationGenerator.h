#import "List.h"

@interface PermutationGenerator : Object {
    int* indexList;  
    int size;
    int currentIndex;
}

- (PermutationGenerator*) initWithSize: (int) size;
- (int) permutationCount;
- (const int*) nextPermutation;
- free;

@end
