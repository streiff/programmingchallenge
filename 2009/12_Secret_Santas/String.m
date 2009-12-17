#import <objc/objc-api.h>
#import <string.h>

#import "String.h"

@implementation String
- (id) initWithCString: (const char*) chars {
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
            cStr[len - 1] == '\n')) {
        cStr[len - 1] = '\0';
        --len;
    }
}

- (int) length {
    return len;
}

- (List*) split: (char) delim {
    List *list = [[List alloc] init];    
    char *buff;
    int pos = len - 1;

    for (int i = pos; i >= 0; --i) {
        if (cStr[i] == delim || i == 0) {
            int size = pos - (cStr[i] == delim ? i : i - 1);
            int startLoc = i + (cStr[i] == delim ? 1 : 0);
            
            buff = objc_malloc(sizeof(char) * (size + 1));
            buff = strncpy(buff, cStr + startLoc, size);
            buff[size] = '\0';
            [list push: [[String alloc] initWithCString: buff]];
            objc_free(buff);
            pos = i - 1;
        }
    }

    return list;
}

- (void) append: (String*) str {
    int newLen = strlen(cStr) + strlen([str cStr]);
    cStr = objc_realloc(cStr, sizeof(char) * (newLen + 1));
    strncpy(cStr + len, [str cStr], [str length]);
    cStr[newLen] = '\0';
    len = newLen;
}

- deepCopy {
    return [[String alloc] initWithCString: [self cStr]];
}

- (BOOL) equals: (String*) s {
    return !strcmp(cStr, [s cStr]);
}

- free {
    if (cStr) {
        objc_free((char*) cStr);
    }
    return [super free];
}

+ (String*) concatenate: (int) count, ... {
    va_list args;
    va_start(args, count);

    String* result = [[String alloc] initWithCString: ""];
    for (int i = 0; i < count; ++i) {
        [result append: va_arg(args, String*)];
    }
    return result;
}
@end
