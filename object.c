#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "list.h"
#include "lang.h"
#include "garbage.h"

#include "object.h"

struct Object* object_init(enum object_type type, union Data data) {
    struct Object* obj = malloc(sizeof(struct Object));
    if(obj == NULL) {
        gc_run();
        obj = malloc(sizeof(struct Object));
        if(obj == NULL) {
            perror("fatal: out of memory\n");
            exit(1);
        }
    }
    obj->type = type;
    obj->data = data;
    obj->marked = 0;
    gc_insert_object(obj);
    return obj;
}

struct Object* object_copy(struct Object* obj) {
    if(obj == NULL) {
        return NULL;
    } else if(obj == true_obj || obj == nil_obj) {
        return obj;
    }

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

int object_equals_value(struct Object* a, struct Object* b) {
    if(a == b) {
        // also covers if both a and b are NULL
        return 1;
    } else if(a == NULL || b == NULL) {
        return 0;
    } else if(a->type != b->type) {
        return 0;
    }

    if(int_type == a->type) {
        return a->data.int_type == b->data.int_type ? 1 : 0;
    } else if(char_type == a->type) {
        return a->data.char_type == b->data.char_type ? 1 : 0;
    } else if(symbol == a->type || string == a->type) {
        return strcmp(a->data.ptr, b->data.ptr) == 0 ? 1 : 0;
    } else {
        return a->data.ptr == b->data.ptr ? 1 : 0;
    }
    // TODO: list comparison
}

void object_mark(struct Object* obj) {
    if(obj == NULL || obj->marked)
        return;

    obj->marked = 1;
    if(obj->type == list_type) {
        struct List* list = (struct List*) obj->data.ptr;
        while(list != NULL) {
            object_mark(list->obj);
            list = list->next;
        }
    } else if(obj->type == func_type) {
        struct Func* func = (struct Func*)obj->data.ptr;
        struct List* args = func->args;
        while(args != NULL) {
            object_mark(args->obj);
            args = args->next;
        }
        object_mark(func->expr);
    }
}

void object_free(struct Object* obj) {
    if(obj == NULL)
        return;

    switch(obj->type) {
        case symbol:
        case string:
            free(obj->data.ptr);
            break;

        case c_fn:
        case nil:
        case true:
            printf("warning: trying to free an unfreeable type\n");
            return;
        default:
            break;
    }

    if(list_type == obj->type) {
        // printf("freeing list\n");
        struct List* list = (struct List*) obj->data.ptr;
        struct List* next = list->next;
        while(list != NULL) {
            free(list);
            list = next;
            if(next != NULL)
                next = next->next;
        }
    } else if(func_type == obj->type) {
        printf("freeing fn\n");
        struct List* list = ((struct Func*) obj->data.ptr)->args;
        struct List* next = list->next;
        while(list != NULL) {
            free(list);
            list = next;
            if(next != NULL)
                next = next->next;
        }
    }
    free(obj);
}

void object_print_type(enum object_type type) {
    switch(type) {
        case symbol:
            printf("symbol");
            break;
        case string:
            printf("string");
            break;
        case char_type:
            printf("char");
            break;
        case int_type:
            printf("int");
            break;
        case float_type:
            printf("float");
            break;
        case list_type:
            printf("list");
            break;
        case c_fn:
            printf("c_fn");
            break;
        case func_type:
            printf("func");
            break;
        case nil:
            printf("nil");
            break;
        case true:
            printf("true");
            break;
        default:
            printf("unknown type");
    }
}

void object_print_string(struct Object* obj) {
    if(obj == NULL) {
        printf("[null object]");
    } else if(list_type == obj->type) {
        printf("(");
        struct List* node = obj->data.ptr;
        if(node->obj != NULL) {
            while(node != NULL) {
                object_print_string(node->obj);
                node = node->next;
                if(node != NULL) {
                    printf(" ");
                }
            }
        }
        printf(")");
    } else if(string == obj->type || symbol == obj->type) {
        // string based types
        printf("%s", (char*) obj->data.ptr);
    } else if(char_type == obj->type) {
        printf("%c", obj->data.char_type);
    } else if(int_type == obj->type) {
        printf("%d", obj->data.int_type);
    } else if(c_fn == obj->type) {
        printf("c_fn<%p>", obj->data.ptr);
    } else if(func_type == obj->type) {
        printf("fn<%p>", obj->data.ptr);
    } else if(true_obj == obj) {
        printf("true");
    } else if(nil_obj == obj) {
        printf("nil");
    } else {
        printf("%p", obj->data.ptr);
    }
}

void object_debug(struct Object* obj, int indent) {
    for(int i = 0; i < indent; i ++) {
        printf("  ");
    }
    printf("adr %p:\n", (void*) obj);

    for(int i = 0; i < indent; i ++) {
        printf("  ");
    }
    object_print_type(obj->type);
    printf("\n");

    for(int i = 0; i < indent; i ++) {
        printf("  ");
    }
    if(obj->marked) {
        printf("mark\n");
    } else {
        printf("no mark\n");
    }

    for(int i = 0; i < indent; i ++) {
        printf("  ");
    }
    switch(obj->type) {
        case symbol:
            printf("data: %s\n", (char*) obj->data.ptr);
            break;
        case string:
            printf("data: %s\n", (char*) obj->data.ptr);
            break;
        case char_type:
            printf("data: %c\n", obj->data.char_type);
            break;
        case int_type:
            printf("data: %d\n", obj->data.int_type);
            break;
        case list_type:
            printf("data:\n");
            // printf("=====BEGIN LIST=====\n");
            list_print(obj->data.ptr, indent + 1);
            // printf("=====END LIST=====\n");
            break;
        default:
            printf("data: %p\n", obj->data.ptr);
    }
}
