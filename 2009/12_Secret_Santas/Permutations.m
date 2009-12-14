#import <objc/objc-api.h>

#import "Permutations.h"

int permutationCount(int n) {
    int i;
    int count;

    if (n < 1) {
        return -1;
    }

    count = 1;
    for (i = n; i > 0; --i) {
        count *= i;
    }
    return count;
}

int* getPermutation(int permNumber, int itemCount) {
    int* perm = objc_malloc(sizeof(int) * (itemCount));
    int i;

    for (i = 0; i < itemCount; ++i) {
        perm[i] = i;
    }

     for (i = 1; i < itemCount; ++i) {
         int temp = perm[permNumber % (i +1 )];
         perm[permNumber % (i + 1)] = perm[i];
         perm[i] = temp;
         permNumber = permNumber / i;
    }
    return perm;
}
