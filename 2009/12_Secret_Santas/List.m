#import "List.h"

@implementation List
- (id) init {
    self = [super init];
    head = NULL;
    return self;
}

- (void) push: (Object*) data {
    if (head == NULL) {
        head = (struct objc_list*) objc_malloc(sizeof(struct objc_list));
        head->head = data;
        head->tail = NULL;
    } else {
        head = list_cons(data, head);
    }
}

- (Object*) at: (int) pos {
    return head == NULL ? 0 : list_nth(pos, head);
}

- (int) length {
    return head == NULL ? 0 : list_length(head);
}
- (void) each: (void(*) (Object*)) func {
    struct objc_list* cell = head;
    while (cell) {
        (*func)(cell->head);
        cell = cell->tail;
    }
}

- free {
    struct objc_list* cell = head;
    while (cell) {
        [(Object*) cell->head free];
        cell = cell->tail;
    }
    list_free(head);
    return [super free];
}

@end
