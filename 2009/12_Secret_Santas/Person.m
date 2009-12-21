#import "Person.h"

@implementation Person
- (id) initWithFirstName: (String*) fn
       lastName: (String*) ln
       email: (String*) e {
    self = [super init];
    firstName = [fn deepCopy];
    lastName = [ln deepCopy];
    email = [e deepCopy];

    return self;
}

- deepCopy {
    return [[Person alloc] initWithFirstName: [firstName deepCopy]
                           lastName: [lastName deepCopy]
                           email: [email deepCopy]];
}

- (String*) toString {
    String *whitespace = [[String alloc] initWithCString: " "];
    String *str = [String concatenate: 5, firstName, 
                                       whitespace,
                                       lastName, 
                                       whitespace,
                                       email];
    [whitespace free];
    return str;
}

- (String*) email {
    return email;
}

- (String*) firstName {
    return firstName;
}

- (String*) lastName {
    return lastName;
}

- (BOOL) isSameFamily: (Person*) p {
    return [lastName equals: [p lastName]];
}

- free {
    [firstName free];
    [lastName free];
    [email free];
    return [super free];
}

@end
