#import "Person.h"

@implementation Person
- (Person*) initWithFirstName: (String*) fn
            lastName: (String*) ln
            email: (String*) e {
    self = [super init];
    firstName = [fn deepCopy];
    lastName = [ln deepCopy];
    email = [e deepCopy];

    return self;
}

- (String*) toString {
    String* whitespace = [[String alloc] initWithCString: " "];
    String* str = [String concatenate: 5, firstName, 
                                       whitespace,
                                       lastName, 
                                       whitespace,
                                       email];
    [whitespace free];
    return str;
}

- free {
    [firstName free];
    [lastName free];
    [email free];
    return [super free];
}

@end
