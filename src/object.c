#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

Object *obj_hashmap_init(int size) {
    HashMap *map = hashmap_init(size);
    union Data dat = { .hashmap = map };
    return obj_init(hashmapt, dat);
}

// releases data associated with object, but not itself
void obj_release(Object *obj) {
    if(!GET_SSTR(obj) && (obj->type == symt || obj->type == strt
                || obj->type == keywordt || obj->type == errt)) {
        free(obj->data.str);
    } else if(obj->type == hashmapt) {
        free(obj->data.hashmap->buckets);
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

int list_length(Object *obj) {
    if(obj->type != listt) {
        return -1;
    }

    if(CAR(obj) == nil_obj && CDR(obj) == nil_obj) {
        return 0;
    }
   
    int len = 0;
    while(obj != nil_obj) {
        if(obj->type != listt) {
            return -1;
        }
        len ++;
        obj = CDR(obj); 
    }

    return len;
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
        case hashmapt:
            hashmap_print(obj->data.hashmap, pretty);
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
            case hashmapt:
                printf("implement hashmap debug\n");
                printf("hashmap<%p>\n", (void *)&obj);
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

unsigned int str_hash(char *str) {
    unsigned int hash = 5381;
    int i = 0;
    while(str[i] != '\0') {
        hash += str[i] * 33;
        i ++;
    }
    return hash;
}

HashMap *hashmap_init(int size) {
    if(size < 8)
        size = 8;
    HashMap *map = malloc(sizeof(HashMap));
    map->capacity = size;
    map->load = 0;

    map->buckets = malloc(sizeof(Object) * size);
    for(int i = 0; i < size; i ++) {
        (map->buckets + i)->type = listt;
        CAR(map->buckets + i) = nil_obj;
        CDR(map->buckets + i) = nil_obj;
    }
    return map;
}

void hashmap_free(HashMap *map) {
    free(map->buckets);
    free(map);
}

void hashmap_resize(HashMap *map) {
    HashMap *newmap = hashmap_init(map->capacity * 2);
    for(int i = 0; i < map->capacity; i ++) {
        Object *list = map->buckets + i;
        while(list != nil_obj) {
            Object *keyval = CAR(list);
            Object *key = CAR(keyval);
            Object *val = CDR(keyval);
            if(list != nil_obj && val != nil_obj) {
                hashmap_set(newmap, key, val);
            }
            list = CDR(list);
        }
    }
    map->capacity = newmap->capacity;

    // swap the buckets
    Object *temp = newmap->buckets;
    newmap->buckets = map->buckets;
    map->buckets = temp;

    // freeing newmap also frees its buckets
    free(newmap);
}

void hashmap_set(HashMap *map, Object *key, Object *value) {
    if((float)map->load / map->capacity > 0.7) {
        hashmap_resize(map);
    }

    unsigned int hash = obj_hash(key) % map->capacity;
    if(CAR(map->buckets + hash) == nil_obj) {
        Object *keyval = cell_init();
        CAR(keyval) = key;
        CDR(keyval) = value;

        CAR(map->buckets + hash) = keyval;
        map->load ++;
    } else {
        Object *list = map->buckets + hash;
        while(list != nil_obj) {
            if(obj_equals(key, CAR(CAR(list)))) {
                // update value
                CDR(CAR(list)) = value;
                return;
            } else if(CDR(list) == nil_obj) {
                // append to end of linked list
                Object *keyval = cell_init();
                CAR(keyval) = key;
                CDR(keyval) = value;

                Object *bucket = cell_init();
                CAR(bucket) = keyval;
                CDR(list) = bucket;
                map->load ++;
                return;
            }
            list = CDR(list);
        }
    }
}

Object *hashmap_get(HashMap *map, Object *key) {
    unsigned int hash = obj_hash(key) % map->capacity;
    Object *list = map->buckets + hash;
    if(CAR(list) == nil_obj) {
        return NULL;
    } else {
        while(list != nil_obj) {
            Object *keyval = CAR(list);

            if(obj_equals(key, CAR(keyval))) {
                Object *val = CDR(keyval);
                return val;
            } else {
                list = CDR(list);
            }
        }
        return NULL;
    }
    return NULL;
}

void hashmap_print(HashMap *map, int pretty) {
    int first = 1;
    printf("{");
    for(int i = 0; i < map->capacity; i ++) {
        Object *keyval = map->buckets + i;
        if(CAR(keyval) == NIL && CDR(keyval) == NIL) {
            continue;
        } else {
            while(keyval != NIL) {
                if(first) {
                    first = 0;
                } else {
                    printf(", ");
                }
                Object *key = CAR(CAR(keyval));
                Object *val = CDR(CAR(keyval));
                obj_print(key, pretty);
                printf(" ");
                obj_print(val, pretty);
                keyval = CDR(keyval);
            }
        }
    }
    printf("}");
}

void hashmap_debug(HashMap *map) {
    for(int i = 0; i < map->capacity; i ++) {
        Object *list = map->buckets + i;
        if(CAR(list) != nil_obj) {
            while(list != nil_obj) {
                Object *key = CAR(CAR(list));
                Object *value = CDR(CAR(list));
                printf("===\n");
                printf("key: ");
                obj_debug(key);
                printf("value: ");
                obj_debug(value);

                list = CDR(list);
            }
        }
    }
}

Envir *envir_init(int size) {
    Envir *envir = malloc(sizeof(Envir));
    envir->map = hashmap_init(size);
    envir->inner = NULL;
    envir->outer = NULL;
    return envir;
}

void envir_free(Envir *envir) {
    hashmap_free(envir->map);
    free(envir);
}

void envir_set(Envir *envir, Object *key, Object *value) {
    hashmap_set(envir->map, key, value);
}

void envir_set_str(Envir *envir, char *key, Object *value) {
    Object *key_obj = obj_init_str(symt, key);
    hashmap_set(envir->map, key_obj, value);
}

Object *envir_get(Envir *envir, Object *key) {
    return hashmap_get(envir->map, key);
}

Object *envir_search(Envir *envir, Object *key) {
    Object *output;
    while((output = envir_get(envir, key)) == NULL) {
        envir = envir->outer;
        if(envir == NULL) {
            return NULL;
        }
    }
    return output;
}
