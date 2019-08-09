#ifndef LA_GARBAGE_H
#define LA_GARBAGE_H

#include "object.h"

/*
 * no meaning to bank size number, might be more efficient to make Bank a total
 * of 4KiB and align to page
 */
#define BANKSIZE 256

typedef struct Bank {
    struct Bank *next;
    Object objs[BANKSIZE];
} Bank;

Bank *all_objects;

void garbage_init();
Object *garbage_alloc();
void garbage_run();
void garbage_shutdown();

#endif
