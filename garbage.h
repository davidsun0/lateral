#ifndef LATERAL_GARBAGE_H
#define LATERAL_GARBAGE_H

void gc_init();

void gc_insert_object(struct Object*);
void gc_scan_stack();

void gc_run();
void gc_delete_everything_yes_im_sure();

#endif
