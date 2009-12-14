#import <objc/objc-api.h>

#import "PermutationGenerator.h"

@implementation PermutationGenerator
- (PermutationGenerator*) initWithSize: (int) sz {
    self = [super init];    
    size = sz;
    indexList = NULL;   
    currentIndex = 0;
    return self;
}

- (int) permutationCount {
    int i;
    int count;

    if (size < 1) {
        return -1;
    }

    count = 1;
    for (i = size; i > 0; --i) {
        count *= i;
    }
    return count;
}

- (const int*) nextPermutation {
    int i;

    if (currentIndex == 0) {
        indexList = objc_malloc(sizeof(int) * (size));

        for (i = 0; i < size; ++i) {
            indexList[i] = i;
        }
        ++currentIndex;
        return indexList;
    }

    if (currentIndex >= [self permutationCount]) {
        return NULL;
    }

    int temp;
    int large = size - 2;
    while (indexList[large] > indexList[large+1]) {
      large--;
    }

    int small = size - 1;
    while (indexList[large] > indexList[small]) {
      small--;
    }

    temp = indexList[small];
    indexList[small] = indexList[large];
    indexList[large] = temp;

    int r = size - 1;
    int s = large + 1;

    while (r > s) {
      temp = indexList[s];
      indexList[s] = indexList[r];
      indexList[r] = temp;
      r--;
      s++;
    }

    ++currentIndex;
    return indexList;

}

- free {
    if (indexList != NULL) {
        objc_free(indexList);
    }
    return [super free];
}


@end
