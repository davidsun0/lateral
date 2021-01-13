#ifndef LATERAL_GARBAGE_H
#define LATERAL_GARBAGE_H

#include "object.h"

void gc_init();

// adds object to pool for automatic collection
void gc_insert_object(struct Object*);

void gc_run();

#endif
