#include <stdlib.h>
#include <stdio.h>

#include "list.h"

#include "object.h"

int object_equals_char(struct Object* obj, char c) {
    if(obj->type != character) {
        return 0;
    } else if(obj->data.character != c) {
        return 0;
    } else {
        return 1;
    }
}

void object_free_member(struct Object* obj) {
    if(obj->type == integer || obj->type == character ||
            obj->type == c_fn) {
        return;
    } else if(obj->type == list_type) {
        list_free(obj->data.ptr);  
    } else if(obj->data.ptr != NULL) {
        free(obj->data.ptr);
        obj->data.ptr = NULL;
    }
}

void object_free(struct Object* obj) {
    object_free_member(obj);
    free(obj);
}

void object_print(struct Object* obj) {
    switch(obj->type) {
        case string:
        case symbol:
            printf("%s", (char*) obj->data.ptr);
            break;
        case character:
            printf("%c", obj->data.character);
            break;
        case integer:
            printf("%d", obj->data.integer);
            break;
        default:
            printf("%p", obj->data.ptr);
    }
}

void object_print_debug(struct Object* obj) {
    printf("obj at %p:\n", (void*) obj);
    switch(obj->type) {
        case symbol:
            printf("type: symbol\n");
            printf("addr: %p\n", obj->data.ptr);
            printf("data: %s\n", (char*) obj->data.ptr);
            break;
        case string:
            printf("type: string\n");
            printf("addr: %p\n", obj->data.ptr);
            printf("data: %s\n", (char*) obj->data.ptr);
            break;
        case character:
            printf("type: character\n");
            printf("data: %c\n", obj->data.character);
            break;
        case integer:
            printf("type: integer\n");
            printf("data: %d\n", obj->data.integer);
            break;
        case list_type:
            printf("type: list\n");
            printf("addr: %p\n", obj->data.ptr);
            printf("data:\n");
            printf("=====BEGIN LIST=====\n");
            list_print(obj->data.ptr);
            printf("=====END LIST=====\n");
            break;
        case c_fn:
            printf("type: c function\n");
            printf("addr: %p\n", obj->data.ptr);
            break;

        case empty:
            printf("EMPTY\n");
            break;
        default:
            printf("unknown type\n");
            printf("data/addr: %p\n", obj->data.ptr);
    }
}
