#ifndef LATERAL_LIST_H
#define LATERAL_LIST_H

#include "object.h"

struct List {
    struct List* next;
    struct Object obj;
};

struct List* list_append_object(struct List*, struct Object*);
struct List* list_append(struct List*, enum object_type, union Data);
void list_print(struct List*);
void list_free(struct List*);

#endif
