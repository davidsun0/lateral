#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "garbage.h"

#include "list.h"

struct Object* list_init() {
    struct Object* output = object_init_type(list_type);
    struct List* list = malloc(sizeof(struct List));
    output->data.ptr = list;
    list->obj = NULL;
    list->next = NULL;
    return output;
}

struct List* list_bare_init() {
    struct List* list = malloc(sizeof(struct List));
    list->obj = NULL;
    list->next = NULL;
    return list;
}

struct List* list_bare_copy(struct Object* obj) {
    if(obj == NULL || obj->type != list_type) {
        return NULL;
    }

    struct List* copy = list_bare_init();
    struct List* source = obj->data.ptr;

    struct List* curr = copy;
    while(source != NULL) {
        curr = list_bare_append(curr, object_copy(source->obj));
        source = source->next;
    }
    return copy;
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

struct List* list_bare_append(struct List* list, struct Object* append) {
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
    return list;
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
    struct Object* append = object_init(type, data);

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

struct Object* list_pop(struct Object* list_obj) {
    struct List* list = (struct List*) list_obj->data.ptr;
    struct Object* output = list->obj;
    list_obj->data.ptr = list->next;
    free(list);
    return output;
}

struct Object* list_get(struct Object* list_obj, int index) {
    struct List* list = (struct List*) list_obj->data.ptr;
    for(int i = 0; i < index; i ++) {
        list = list->next;
        if(list == NULL) {
            return NULL;
        }
    }
    return list->obj;
}

int list_bare_length(struct List* list) {
    if(list == NULL || list->obj == NULL) {
        return 0;
    }

    int length = 0;
    while(list != NULL) {
        list = list->next;
        length ++;
    }
    return length;
}

int list_length(struct Object* obj) {
    if(obj->type != list_type) {
        return -1;
    } else {
        return list_bare_length(obj->data.ptr);
    }
}

int list_is_empty(struct List* list) {
    if(list->next == NULL && list->obj == NULL) {
        return 1;
    } else {
        return 0;
    }
}
