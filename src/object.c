#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hash.h"
#include "reader.h"
#include "garbage.h"
#include "core.h"

#include "object.h"

char *obj_string(Object *o) {
    if(GET_SSTR(o)) {
        return o->data.short_str;
    } else {
        return o->data.str;
    }
}

char *type_to_str(obj_type type) {
    switch(type) {
        case empty:     return "empty";
        case symt:      return "symt";
        case strt:      return "strt";
        case keywordt:  return "keywordt";
        case intt:      return "intt";
        case floatt:    return "floatt";
        case listt:     return "listt";
        case natfnt:    return "natfnt";
        case macrot:    return "macrot";
        case errt:      return "errt";
        default:        return "corrupted";
    }
}


Object *obj_init(obj_type type, union Data data) {
    Object *obj = garbage_alloc();
    obj->type = type;
    obj->data = data;
    obj->flags = 0;
    return obj;
}

Object *cell_init() {
    union Data dat = { .cell = { nil_obj, nil_obj} };
    return obj_init(listt, dat);
}

Object *obj_init_str_len(obj_type type, char *str, int len) {
    if(len < 16) {
        // store short strings within the data struct
        union Data dat;
        strncpy(dat.short_str, str, len);
        dat.short_str[len] = '\0';
        Object *o = obj_init(type, dat);
        SET_SSTR(o);
        return o;
    } else {
        char *newstr = malloc(len + 1);
        strncpy(newstr, str, len);
        newstr[len] = '\0';
        union Data dat = { .str = newstr };
        return obj_init(type, dat);
    }
}

Object *obj_init_str(obj_type type, char *str) {
    return obj_init_str_len(type, str, strlen(str));
}

Object *err_init(char *str) {
    return obj_init_str(errt, str);
}

// releases data associated with object, but not itself
void obj_release(Object *obj) {
    if(!GET_SSTR(obj) && (obj->type == symt || obj->type == strt
                || obj->type == keywordt || obj->type == errt)) {
        free(obj->data.str);
    }
}

void obj_mark(Object *obj) {
    if(GET_MARK(obj))
        return;

    SET_MARK(obj);
    if(obj->type == listt) {
        obj_mark(CAR(obj));
        obj_mark(CDR(obj));
    } else if(obj->type == fnt || obj->type == macrot) {
        obj_mark(obj->data.func.args);
        obj_mark(obj->data.func.expr);
    }
}

unsigned int obj_hash(Object *obj) {
    if(obj->type == symt || obj->type == strt || obj->type == keywordt
            || obj->type == errt) {
        return str_hash(obj_string(obj));
    } else {
        printf("hash function not implemented for %s type\n",
                type_to_str(obj->type));
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
            return strcmp(obj_string(a), obj_string(b)) == 0;
        case intt:
            return a->data.int_val == b->data.int_val;
        case floatt:
            return a->data.float_val == b->data.float_val;
        default:
            // printf("equality not implemented for this type\n");
            printf("equality not implemented for %s type\n",
                    type_to_str(a->type));
            return 0;
    }
}

Object *list_length(Object *obj) {
    int len = 0;
    if(obj->type != listt) {
        return err_init("type error: object is not a list");
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
    if(list == NULL) {
        Object *ret = cell_init();
        CAR(ret) = obj;
        CDR(ret) = nil_obj;
        return ret;
    }

    if(list->type != listt) {
        printf("warning: trying to append to object that is not a list\n");
        return NULL;
    }

    if(list == nil_obj) {
        printf("trying to append to nil?\n");
        return NULL;
    }

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

int obj_eq_sym(Object *obj, char *str) {
    if(obj->type != symt) {
        return 0;
    }

    char *ostr = obj_string(obj);
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
                printf("%s", obj_string(obj));
            } else {
                printf("\"%s\"", obj_string(obj));
            }
            break;
        case symt:
        case keywordt:
        case errt:
            printf("%s", obj_string(obj));
            break;
        case intt:
            printf("%d", obj->data.int_val);
            break;
        case listt:
            printf("(");
            while(obj != nil_obj) {
                obj_print(CAR(obj), pretty);
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
    if(obj == NULL) {
        return;
    }

    for(int i = 0; i < indt; i ++) {
        printf("  ");
    }

    printf("flags: %d\n", obj->flags);
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
                printf("sym: %s\n", obj_string(obj));
                break;
            case strt:
                printf("str: %s\n", obj_string(obj));
                break;
            case keywordt:
                printf("key: %s\n", obj_string(obj));
                break;
            case errt:
                printf("err: %s\n", obj_string(obj));
                break;
            case intt:
                printf("int: %d\n", obj->data.int_val);
                break;
            case listt:
                printf("list<%p>\n", obj->data.ptr);
                while(obj != nil_obj) {
                    obj_debug0(obj->data.cell.car, indt + 1);
                    obj = obj->data.cell.cdr;
                }
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
