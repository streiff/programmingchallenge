#import <objc/Object.h>

#import "List.h"

@interface String : Object {
    int len;
    char* cStr;
}

- (String*) initWithCString: (const char*) cStr;
- (char*) cStr;
- (int) length;
- (void) chomp;
- (void) append: (String*) str;
- (List*) split: (char) delim;
- free;
+ (String*) concatenate: (int) count, ...;

@end
