#import <objc/objc-api.h>
#import <string.h>

#import "String.h"

@implementation String
- (String*) initWithCString: (const char*) chars {
    self = [super init];
    len = strlen(chars);
    cStr = objc_malloc(sizeof(char) * (len + 1));
    cStr = strncpy(cStr, chars, len);
    cStr[len] = '\0';
    return self;
}

- (char*) cStr {
    return cStr;
}

- (void) chomp {
    while (len > 0 && (
            cStr[len - 1] == '\r'  ||
            cStr[len - 1] == '\l'  ||
            cStr[len - 1] == '\n')) {
        cStr[len - 1] = '\0';
        --len;
    }
}

- (int) length {
    return len;
}

- (List*) split: (char) delim {
    List* list = [[List alloc] init];    
    int i, j, pos;
    char* buff;

    pos = 0;
    for (i = 0, j = [self length]; i < j; ++i) {
        if (cStr[i] == delim) {
            buff = objc_malloc(sizeof(char) * (i - pos + 1));
            buff = strncpy(buff, cStr + pos, i - pos);
            buff[i - pos] = '\0';
            [list push: [[String alloc] initWithCString: buff]];
            objc_free(buff);
            pos = i + 1;
        }
    }

    if (pos <= [self length]) {
        buff = objc_malloc(sizeof(char) * (i - pos + 1));
        buff = strncpy(buff, cStr + pos, i - pos);
        buff[i - pos] = '\0';
        [list push: [[String alloc] initWithCString: buff]];
        objc_free(buff);
    }

    return list;
}
- deepCopy {
    return [[String alloc] initWithCString: [self cStr]];
}


- free {
    objc_free((char*) cStr);
    return [super free];
}
@end
