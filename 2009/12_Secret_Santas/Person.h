#import <objc/Object.h>
#import "String.h"

@interface Person : Object {
    String* firstName;
    String* lastName;
    String* email;
}

- (id) initWithFirstName: (String*) firstName
            lastName: (String*) lastName
            email: (String*) email;

- (String*) toString;
- (String*) email;
- (String*) firstName;
- (String*) lastName;
- (BOOL) isSameFamily: (Person*) p;
- free;

@end
