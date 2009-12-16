#import <objc/objc-api.h>

#import "PermutationGenerator.h"

@implementation PermutationGenerator
- (PermutationGenerator*) initWithSize: (int) sz {
    self = [super init];    
    size = sz;
    indexList = NULL;   
    noMoreResults = sz <= 0;
    return self;
}

// algorithm adapted from:
// Kenneth H. Rosen - Discrete Mathematics and Its Applications
- (const int*) nextPermutation {
    if (noMoreResults) {
        return NULL;
    }

    int i;
    if (indexList == NULL) {
        indexList = objc_malloc(sizeof(int) * (size));

        for (i = 0; i < size; ++i) {
            indexList[i] = i;
        }
        return indexList;
    }

    int large = size - 2;
    while (indexList[large] > indexList[large+1]) {
        large--;
    }

    int small = size - 1;
    while (indexList[large] > indexList[small]) {
        small--;
    }

    int temp = indexList[small];
    indexList[small] = indexList[large];
    indexList[large] = temp;

    int r = size - 1;
    int s = large + 1;

    while (r > s) {
        temp = indexList[s];
        indexList[s] = indexList[r];
        indexList[r] = temp;
        --r;
        ++s;
    }

    // check to see if the list is the same as the first one.
    // we can't do a counter on this because factorials grow too large
    // too fast.
    BOOL isFirst = YES;
    for (i = 0; i < size; ++i) {
        if (indexList[i] != i) {
            isFirst = NO;
        }
    }

    if (isFirst) {
        noMoreResults = YES;
        return NULL;
    } else {
        return indexList;
    }
}

- free {
    if (indexList != NULL) {
        objc_free(indexList);
    }
    return [super free];
}
@end
