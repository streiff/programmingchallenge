#import <objc/Object.h>

#import "List.h"

@interface String : Object {
    int len;
    char* cStr;
}

- (id) initWithCString: (const char*) cStr;
- (char*) cStr;
- (int) length;
- (void) chomp;
- (void) append: (String*) str;
- (List*) split: (char) delim;
- (BOOL) equals: (String*) s;
- free;

+ (String*) concatenate: (int) count, ...;

@end
