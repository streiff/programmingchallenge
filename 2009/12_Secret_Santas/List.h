#import <objc/Object.h>
#import <objc/objc-api.h>
#import <objc/objc-list.h>

@interface List : Object {
    struct objc_list *head;
}

- (id) init;
- (Object*) at: (int) pos;
- (void) push: (Object*) data;
- (int) length;

- (void) each: (void(*) (Object*)) func;
- free;

@end
