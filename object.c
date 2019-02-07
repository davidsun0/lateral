#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "list.h"

#include "object.h"

struct Object* object_init(enum object_type type, union Data data) {
    struct Object* obj = malloc(sizeof(struct Object));
    obj->type = type;
    obj->data = data;
    obj->quote = none;
    return obj;
}

struct Object* object_copy(struct Object* obj) {
    if(obj == NULL)
        return NULL;

    union Data dat;
    if(obj->type == char_type || obj->type == int_type || obj->type == c_fn) {
        // direct copy
        dat = obj->data;
    } else if(obj->type == symbol || obj->type == string) {
        // string copy
        int length = strlen(obj->data.ptr);
        char* str = malloc(sizeof(char) * (length + 1));
        strcpy(str, obj->data.ptr);
        dat.ptr = str;
    } else if(obj->type == list_type) {
        struct Object* clone = list_init();
        struct List* list = obj->data.ptr;
        while(list != NULL) {
            list_append_object(clone, object_copy(list->obj));
            list = list->next;
        }
        return clone;
    } else if(obj->type == func_type) {
        printf("implement object_copy for functions\n");
        return NULL;
    }
    // copy structure + recurse for list, func
    return object_init(obj->type, dat);
}

int object_equals_char(struct Object* obj, char c) {
    if(obj != NULL && obj->type == char_type && obj->data.char_type == c) {
        return 1;
    } else {
        return 0;
    }
}

int object_equals_symbol(struct Object* obj, char* str) {
    if(obj != NULL && obj->type == symbol &&
            strcmp(obj->data.ptr, str) == 0) {
        return 1;
    } else {
        return 0;
    }
}

/*
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
*/
/*
void object_free(struct Object* obj) {
    object_free_member(obj);
    free(obj);
}
*/

void object_print_string(struct Object* obj) {
    if(obj == NULL) {
        printf("[null object]");
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
    } else if(obj->type == c_fn) {
        printf("c_fn<%p>", obj->data.ptr);
    } else if(obj->type == func_type) {
        printf("fn<%p>", obj->data.ptr);
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

        default:
            printf("unknown type\n");
            printf("data/addr: %p\n", obj->data.ptr);
    }
}
