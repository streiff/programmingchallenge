#import "List.h"

/* Calculator of the number of permutations in a set of size n. 
 *
 * Param: n - Number of items in the permutation set.
 * Returns: Number of unique permutations of a set of n items.
 */
int permutationCount(int n);

/* Creates an int array of indexes of permutation number permNumber
 * of a collection of itemCount elements.
 *
 * Param: permNumber: A number from 0 to (itemCount!-1) inclusive.
 *        itemCount: Number of items in the permutaion set.
 * Returns: An int array of indexes for permutation number permNumber
 *          and a permutation set of itemCount items.
 *
 * Note: The caller of this method is responsible for calling objc_free
 *       on the returned array.
 */
int* getPermutation(int permNumber, int itemCount);
