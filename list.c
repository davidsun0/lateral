#include <stdlib.h>
#include <stdio.h>

#include "list.h"
#include "object.h"

struct Object* list_init() {
    struct List* list = malloc(sizeof(struct List));
    list->obj = NULL;
    list->next = NULL;
    union Data data;
    data.ptr = list;
    return object_init(list_type, data);
}

void list_append_object(struct Object* list_obj, struct Object* append) {
    if(list_obj == NULL || list_obj->type != list_type) {
        return;
    }
    struct List* list = (struct List*) list_obj->data.ptr;
    while(list->next != NULL) {
        list = list->next;
    }
    if(list->obj == NULL) {
        list->obj = append;
    } else {
        struct List* node = malloc(sizeof(struct List));
        node->obj = append;
        node->next = NULL;
        list->next = node;
    }
}

void list_append(struct Object* list_obj, enum object_type type,
        union Data data) {
    if(list_obj == NULL || list_obj->type != list_type) {
        return;
    }
    struct Object* append = malloc(sizeof(struct Object));
    append->type = type;
    append->data = data;
    list_append_object(list_obj, append);
}

void list_print(struct List* list) {
    while(list != NULL) {
        printf("node adr: %p\n", (void*) list);
        if(list->obj != NULL) {
            object_print_debug(list->obj);
        } else {
            printf("node has null object");
        }
        printf("\n");
        list = list->next;
    }
}

void list_free(struct List* list) {
    struct List* next;
    while(list != NULL) {
        next = list->next;
        object_free_member(list->obj);
        free(list);
        list = next;
    }
}
