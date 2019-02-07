#ifndef LATERAL_LIST_H
#define LATERAL_LIST_H

#include "object.h"

struct List {
    struct List* next;
    struct Object* obj;
};

// struct List* list_append_object(struct List*, struct Object*);
// struct List* list_append(struct List*, enum object_type, union Data);
struct Object* list_init();
void list_append_object(struct Object*, struct Object*);
void list_append(struct Object*, enum object_type, union Data);

int list_length(struct List*);
void list_print(struct List*);
void list_free(struct List*);

#endif
