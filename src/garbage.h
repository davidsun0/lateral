#ifndef LA_GARBAGE_H
#define LA_GARBAGE_H

#include "object.h"

#define BANKSIZE 256

// List *all_objects;

typedef struct Bank {
    struct Bank *next;
    Object objs[BANKSIZE];
} Bank;

Bank *all_objects;

void garbage_init();
Object *garbage_alloc();
// void garbage_insert(Object *);
void garbage_run();
void garbage_shutdown();

#endif
