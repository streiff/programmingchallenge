#import <objc/Object.h>

#import "String.h"

@interface FileReader : Object {
    String* filename;
    FILE* file;
    BOOL endOfFile;
}

- (id) initWithFilename: (String*) filename;
- (String*) readLine;
- free;

@end
