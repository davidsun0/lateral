#include <stdlib.h>
#include <stdio.h>

#include "object.h"

void object_free_member(struct Object* obj) {
    if(obj->type == integer || obj->type == character) {
        return;
    } else if(obj->data.ptr != NULL) {
        free(obj->data.ptr);
        obj->data.ptr = NULL;
    }
}

void object_free(struct Object* obj) {
    object_free_member(obj);
    free(obj);
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

        case empty:
            printf("EMPTY\n");
            break;
        default:
            printf("unknown type\n");
            printf("data/addr: %p\n", obj->data.ptr);
    }
}
