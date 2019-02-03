#ifndef LATERAL_LIST_H
#define LATERAL_LIST_H

struct List{
    struct List* next;
    void* data;
} List;

struct List* list_append(struct List*, void*);
void list_print(struct List*);

#endif
