#ifndef LA_GARBAGE_H
#define LA_GARBAGE_H

#include "object.h"

List *all_objects;

void garbage_init();
void garbage_insert(Object *);
void garbage_run();

#endif
