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

struct List* list_bare_init() {
    struct List* list = malloc(sizeof(struct List));
    list->obj = NULL;
    list->next = NULL;
    return list;
}

/*
void list_free(struct List* list) {
    struct List* next;
    while(list != NULL) {
        next = list->next;
        // object_free_member(list->obj);
        free(list);
        list = next;
    }
}
*/

struct List* list_copy_struct(struct List* source) {
    struct List* dest = malloc(sizeof(struct List));
    struct List* dest_node = dest;
    dest_node->obj = NULL;
    while(source != NULL) {
        if(dest_node->obj != NULL) {
            struct List* dest_cpy = malloc(sizeof(struct List));

            dest_cpy->next = NULL;
            dest_cpy->obj = source->obj;

            dest_node->next = dest_cpy;
            dest_node = dest_node->next;
        } else {
            dest_node->obj = source->obj;
            dest_node->next = NULL;
        }
        source = source->next;
    }
    return dest;
}

void list_bare_prepend(struct List** list, struct Object* prepend) {
    if((*list)->obj == NULL) {
        (*list)->obj = prepend;
    } else {
        struct List* node = malloc(sizeof(struct List));
        node->obj = prepend;
        node->next = *list;
        *list = node;
    }
}

void list_prepend_object(struct Object* list_obj, struct Object* prepend) {
    if(list_obj == NULL || list_obj->type != list_type) {
        return;
    }
    struct List* list = (struct List*) list_obj->data.ptr;
    if(list->obj == NULL) {
        list->obj = prepend;
    } else {
        struct List* node = malloc(sizeof(struct List));
        node->obj = prepend;
        node->next = list;
        list_obj->data.ptr = node;
    }
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

int list_length(struct List* list) {
    int length = 0;
    while(list != NULL) {
        list = list->next;
        length ++;
    }
    return length;
}

void list_print(struct List* list, int indent) {
    while(list != NULL) {
        for(int i = 0; i < indent; i ++) {
            printf("  ");
        }
        printf("node adr: %p\n", (void*) list);

        if(list->obj != NULL) {
            object_debug(list->obj, indent + 1);
        } else {
            for(int i = 0; i < indent; i ++) {
                printf("  ");
            }
            printf("node has null object");
        }
        printf("\n");
        list = list->next;
    }
}
