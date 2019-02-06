#include <stdlib.h>
#include <stdio.h>

#include "list.h"

#include "object.h"

struct Object* object_init(enum object_type type, union Data data) {
    struct Object* obj = malloc(sizeof(struct Object));
    obj->type = type;
    obj->data = data;
    return obj;
}

int object_equals_char(struct Object* obj, char c) {
    if(obj != NULL && obj->type == char_type && obj->data.char_type == c) {
        return 1;
    } else {
        return 0;
    }
}

void object_free_member(struct Object* obj) {
    if(obj->type == int_type || obj->type == char_type ||
            obj->type == c_fn) {
        return;
    } else if(obj->type == list_type) {
        list_free(obj->data.ptr);  
    } else if(obj->data.ptr != NULL) {
        free(obj->data.ptr);
        obj->data.ptr = NULL;
    }
}

/*
void object_free(struct Object* obj) {
    object_free_member(obj);
    free(obj);
}
*/

void object_print_string(struct Object* obj) {
    if(obj == NULL) {
        printf("[null object]\n");
    } else if(obj->type == list_type) {
        printf("(");
        struct List* node = obj->data.ptr;
        while(node != NULL) {
            object_print_string(node->obj);
            node = node->next;
            if(node != NULL) {
                printf(" ");
            }
        }
        printf(")");
    } else if(obj->type == string || obj->type == symbol) {
        // string based types
        printf("%s", (char*) obj->data.ptr);
    } else if(obj->type == char_type) {
        printf("%c", obj->data.char_type);
    } else if(obj->type == int_type) {
        printf("%d", obj->data.int_type);
    } else {
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
        case char_type:
            printf("type: character\n");
            printf("data: %c\n", obj->data.char_type);
            break;
        case int_type:
            printf("type: integer\n");
            printf("data: %d\n", obj->data.int_type);
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
