#import <stdio.h>

#import "FileReader.h"
#import "String.h"

@implementation FileReader

-(id) initWithFilename: (String*) f {
    self = [super init];
    filename = [f deepCopy];
    endOfFile = NO;
    return self;
}

-(String*) readLine {
    char line[2048];

    if (endOfFile) {
        return nil;
    }

    if (file == NULL) {
        file = fopen([filename cStr], "r");
    }

    if (file == NULL) {
        perror([filename cStr]);
        endOfFile = YES;
        return nil;
    }

    if (fgets(line, sizeof line, file) == NULL) {
        fclose(file);
        endOfFile = YES;
        return nil;
    }
    
    String* s = [[String alloc] initWithCString: line];
    [s chomp];
    return s; 
}

- free {
    if (filename) {
        [filename free];
    }
    return [super free];
}

@end
