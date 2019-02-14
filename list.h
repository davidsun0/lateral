#ifndef LATERAL_LIST_H
#define LATERAL_LIST_H

#include "object.h"

struct List {
    struct List* next;
    struct Object* obj;
};

struct Object* list_init();
struct List* list_bare_init();

struct List* list_bare_copy(struct Object*);

void list_prepend_object(struct Object*, struct Object*);
void list_bare_prepend(struct List**, struct Object*);

void list_append_object(struct Object*, struct Object*);
void list_append(struct Object*, enum object_type, union Data);
struct List* list_bare_append(struct List*, struct Object*);

int list_length(struct List*);
void list_print(struct List*, int);

#endif
