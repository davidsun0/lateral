#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hash.h"
#include "reader.h"
#include "garbage.h"
#include "core.h"

#include "object.h"

Object *obj_init(obj_type type, union Data data) {
    Object *obj = garbage_alloc();
    obj->type = type;
    obj->data = data;
    obj->marked = 0;
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
        default:
            break;
    }

    free(obj);
}

// releases data associated with object, but not itself
void obj_release(Object *obj) {
    if(obj == NULL)
        return;

    switch(obj->type) {
        case symt:
        case strt:
        case keywordt:
        case errt:
            free(obj->data.ptr);
            break;
        default:
            break;
    }
}


Object *cell_init() {
    union Data dat = { .cell = { nil_obj, nil_obj} };
    return obj_init(listt, dat);
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
        obj_mark(CAR(obj));
        obj_mark(CDR(obj));
    } else if(obj->type == fnt || obj->type == macrot) {
        obj_mark(obj->data.func.args);
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

    if(a->type != b->type) {
        return 0;
    }

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
    if(obj->type == listt && CAR(obj) == nil_obj && CDR(obj) == NULL) {
        return 1;
    } else {
        return 0;
    }
}

Object *list_length(Object *obj) {
    int len = 0;
    if(obj->type != listt) {
        return err_init("type error: object is not a list");
    }

    if(CAR(obj) == nil_obj) {
        union Data dat = { .int_val = 0 };
        return obj_init(intt, dat);
    }

    while(obj != nil_obj) {
        if(obj->type != listt) {
            return err_init("type error: object not proper list");
        }
        len ++;
        obj = CDR(obj); 
    }

    union Data dat = { .int_val = len };
    return obj_init(intt, dat);
}

Object *list_append(Object *list, Object *obj) {
    if(list->type != listt) {
        printf("warning: trying to append to object that is not a list\n");
        return NULL;
    }

    if(list == nil_obj) {
        printf("trying to append to nil?\n");
        return NULL;
    }

    if(CAR(list) == nil_obj) {
        CAR(list) = obj;
        return list;
    } else {
        while(CDR(list) != nil_obj) {
            list = CDR(list);
            if(list->type != listt) {
                return err_init("error: object is not a proper list");
            }
        }
        Object *newcell = cell_init();
        CAR(newcell) = obj;
        CDR(list) = newcell;
        return newcell;
    }
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
            while(obj != nil_obj) {
                if(CAR(obj) != nil_obj) {
                    obj_print(CAR(obj), pretty);
                }
                if(CDR(obj) != nil_obj) {
                    printf(" ");
                }
                obj = CDR(obj);
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
                obj_debug0(obj->data.cell.car, indt + 1);
                obj_debug0(obj->data.cell.cdr, indt);
                break;
            case natfnt:
                printf("natfn<%p>\n", obj->data.ptr);
                break;
            case fnt:
                printf("fn<%p>\n", obj->data.ptr);
                break;
            case macrot:
                printf("macro<%p>:\n", obj->data.ptr);
                obj_debug0(obj->data.func.args, indt + 1);
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
