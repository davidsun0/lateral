#ifndef LATERAL_GARBAGE_H
#define LATERAL_GARBAGE_H

void gc_init();

// checked malloc which runs garbage collection when appropriate
void* gc_malloc(size_t);

// adds object to pool for automatic collection
void gc_insert_object(struct Object*);

void gc_run();

#endif
