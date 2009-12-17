#import <stdio.h>
#import <stdlib.h>
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

- (void) remove: (Object*) data {
    struct objc_list *cell = head;
    struct objc_list *prevCell = NULL;

    while (cell) {
        if (cell->head == data) {
            if (cell == head) {
                // move the head
                head = cell->tail;
                objc_free(cell);
                cell = head;
            } else if (cell->tail) {
                // copy the next cell into this one
                struct objc_list *tmp = cell->tail;
                cell->head = tmp->head;
                cell->tail = tmp->tail;
                objc_free(tmp);
            } else {
                // at the end.  remove it and reset the end
                prevCell->tail = NULL;
                objc_free(cell);
            }
            prevCell = cell; 
        } else {
            prevCell = cell; 
            cell = cell->tail;
        }
    }
}


- (Object*) at: (int) pos {
    return head == NULL ? 0 : list_nth(pos, head);
}

- (void) shuffle {
    struct objc_list *cell = NULL;

    while ([self length] > 0) {
        int randIndex = random() % [self length];
        Object *o = [self at: randIndex];

        if (cell == NULL) {
            cell = (struct objc_list*) objc_malloc(sizeof(struct objc_list));
            cell->head = o;
            cell->tail = NULL;
        } else {
            cell = list_cons(o, cell);
        }
        [self remove: o];
    }

    head = cell;
}

- (void) swap: (int) n1 with: (int) n2 {
    if (n1 >= [self length] || n2 >= [self length]) {
        printf("an index out of bounds %i, %i [size %i]\n", n1, n2, [self length]);
        return;
    }

    Object *temp;
    struct objc_list *p = head;
    struct objc_list *cell1 = NULL;
    struct objc_list *cell2 = NULL;
    int i = 0;

    do {
        if (i == n1) {
            cell1 = p;
        }
        if (i == n2) {
            cell2 = p;
        }
        p = p->tail;
        ++i;
    } while (p && (!cell1 || !cell2));

    if (cell1 && cell2) {
        temp = cell1->head;
        cell1->head = cell2->head;
        cell2->head = temp;
    }
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
    struct objc_list *cell = head;
    while (cell) {
        [(Object*) cell->head free];
        cell = cell->tail;
    }
    list_free(head);
    return [super free];
}

@end
