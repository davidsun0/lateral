#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "list.h"
#include "hash.h"
#include "reader.h"
#include "core.h"
#include "garbage.h"

#include "object.h"

Object *obj_init(obj_type type, union Data data) {
    Object *obj = malloc(sizeof(Object));
    if(!obj) {
        fprintf(stderr, "error: failed to allocate object\n");
        exit(1);
    }
    obj->type = type;
    obj->data = data;

    garbage_insert(obj);
    return obj;
}

void obj_free(Object *obj) {
    if(obj == NULL)
        return;

    switch(obj->type) {
        case symt:
        case strt:
        case keywordt:
        case errt:
            free(obj->data.ptr);
            break;
        case listt:
            list_free(obj->data.ptr);
        default:
            break;
    }

    free(obj);
}

Object *err_init(char *str) {
    char *err = la_strdup(str);
    union Data dat = { .ptr = err };
    return obj_init(errt, dat);
}

void obj_mark(Object *obj) {
    if(obj->marked)
        return;

    obj->marked = 1;
    if(obj->type == listt) {
        List *list = obj->data.ptr;
        if(list->obj != NULL) {
            while(list != NULL) {
                obj_mark(list->obj);
                list = list->next;
            }
        }
    } else if(obj->type == fnt || obj->type == macrot) {
        List *list = obj->data.func.args;
        if(list->obj != NULL) {
            while(list != NULL) {
                obj_mark(list->obj);
                list = list->next;
            }
        }
        obj_mark(obj->data.func.expr);
    }
}

unsigned int obj_hash(Object *obj) {
    switch(obj->type) {
        case symt:
        case strt:
        case keywordt:
        case errt:
            return str_hash(obj->data.ptr);
        default:
            printf("hash function not implemented for this type\n");
            return 0;
    }
}

int obj_equals(Object *a, Object *b) {
    if(a == b)
        return 1;

    if(a->type != b->type)
        return 0;

    switch(a->type) {
        case symt:
        case strt:
        case keywordt:
        case errt:
            return strcmp(a->data.ptr, b->data.ptr) == 0;
        case intt:
            return a->data.int_val == b->data.int_val;
        case floatt:
            return a->data.float_val == b->data.float_val;
        default:
            printf("equality not implemented for this type\n");
            return 0;
    }
}

int obj_is_empty_list(Object *obj) {
    if(obj->type != listt) {
        return 0;
    }
    List *list = obj->data.ptr;
    if(list->obj == NULL) {
        return 1;
    }
    return 0;
}

int obj_eq_sym(Object *obj, char *str) {
    if(obj->type != symt) {
        return 0;
    }

    char *ostr = (char *)obj->data.ptr;
    return strcmp(ostr, str) == 0;
}

void obj_print(Object *obj, int pretty) {
    if(obj == nil_obj) {
        printf("nil");
        return;
    } else if(obj == tru_obj) {
        printf("t");
        return;
    }

    switch(obj->type) {
        case strt:
            if(pretty) {
                printf("%s", (char *)obj->data.ptr);
            } else {
                printf("\"%s\"", (char *)obj->data.ptr);
            }
            break;
        case symt:
        case keywordt:
        case errt:
            printf("%s", (char *)obj->data.ptr);
            break;
        case intt:
            printf("%d", obj->data.int_val);
            break;
        case listt:
            printf("(");
            List *list = obj->data.ptr;
            while(list != NULL) {
                if(list->obj != NULL) {
                    obj_print(list->obj, pretty);
                }
                if(list->next != NULL) {
                    printf(" ");
                }
                list = list->next;
            }
            printf(")");
            break;
        case natfnt:
            printf("natfn<%p>", obj->data.ptr);
            break;
        case fnt:
            printf("fn<%p>", obj->data.ptr);
            break;
        case macrot:
            printf("macro<%p>", obj->data.ptr);
            break;
        default:
            printf("object@<%p>", (void *)obj);
    }
}

void obj_debug0(Object *obj, int indt);

void list_debug0(List *list, int indt) {
    while(list != NULL) {
        obj_debug0(list->obj, indt);
        list = list->next;
    }
}

void obj_debug0(Object *obj, int indt) {
    for(int i = 0; i < indt; i ++) {
        printf("  ");
    }

    if(obj == nil_obj) {
        printf("nil_obj\n");
        return;
    } else if(obj == tru_obj) {
        printf("t_obj\n");
        return;
    } else if(obj == NULL) {
        printf("NULL\n");
    } else {
        switch(obj->type) {
            case symt:
                printf("sym: %s\n", (char *)obj->data.ptr);
                break;
            case strt:
                printf("str: %s\n", (char *)obj->data.ptr);
                break;
            case keywordt:
                printf("key: %s\n", (char *)obj->data.ptr);
                break;
            case errt:
                printf("err: %s\n", (char *)obj->data.ptr);
                break;
            case intt:
                printf("int: %d\n", obj->data.int_val);
                break;
            case listt:
                printf("list<%p>\n", obj->data.ptr);
                List *list = obj->data.ptr;
                /*
                while(list != NULL) {
                    obj_debug0(list->obj, indt + 1);
                    list = list->next;
                }
                */
                list_debug0(list, indt + 1);
                break;
            case natfnt:
                printf("natfn<%p>\n", obj->data.ptr);
                break;
            case fnt:
                printf("fn<%p>\n", obj->data.ptr);
                break;
            case macrot:
                printf("macro<%p>:\n", obj->data.ptr);
                list_debug0(obj->data.func.args, indt + 1);
                obj_debug0(obj->data.func.expr, indt + 1);
                break;
            default:
                printf("unimplemented\n");
        }
    }
}

void obj_debug(Object *obj) {
    obj_debug0(obj, 0);
}
