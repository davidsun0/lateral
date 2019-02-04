#include <stdlib.h>
#include <stdio.h>

#include "list.h"
#include "object.h"

struct List* list_append_object(struct List* list, struct Object* obj) {
    if(obj == NULL) {
        return list;
    }

    // insert if current node's object is empty
    if(list->obj.type == empty) {
        list->obj.type = obj->type;
        list->obj.data = obj->data;
        return list;
    } else {
        while(list->next != NULL) {
            list = list->next;
        }
        struct List* node = malloc(sizeof(struct List));
        node->obj.type = obj->type;
        node->obj.data = obj->data;
        node->next = NULL;
        list->next = node;
        return node;
    }
}

struct List* list_append(struct List* list, enum object_type type,
        union Data data) {
    if(list->obj.type == empty) {
        list->obj.type = type;
        list->obj.data = data;
        return list;
    } else {
        while(list->next != NULL) {
            list = list->next;
        }

        struct List* node = malloc(sizeof(struct List));
        node->obj.type = type;
        node->obj.data = data;

        node->next = NULL;
        list->next = node;
        return node;
    }
}

void list_print(struct List* list) {
    while(list != NULL) {
        printf("node adr: %p\n", (void*) list);
        object_print_debug(&list->obj);
        printf("\n");
        list = list->next;
    }
}

void list_free(struct List* list) {
    struct List* next;
    while(list != NULL) {
        next = list->next;
        object_free(&list->obj);
        free(list);
        list = next;
    }
}
