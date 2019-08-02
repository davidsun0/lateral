#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "eval.h"
#include "reader.h"

#include "core.h"

Object *la_sum(Object *list) {
    int sum = 0;
    while(list != nil_obj) {
        Object *obj = CAR(list);
        if(obj->type == intt) {
            sum += obj->data.int_val;
        } else {
            return err_init("type error");
        }
        list = CDR(list);
    }
    union Data dat = { .int_val = sum };
    return obj_init(intt, dat);
}

Object *la_lt(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));
    if(a->type != intt || b->type != intt) {
        return err_init("type error");
    } else if(a->data.int_val < b->data.int_val) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

Object *la_eq(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));
    if(a->type != intt || b->type != intt) {
        return err_init("type error");
    } else if(a->data.int_val == b->data.int_val) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

/*
 * LIST FUNCTIONS
 *
 */
Object *la_is_nil(Object *list) {
    if(CAR(list) == nil_obj) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

Object *la_is_empty(Object *list) {
    Object *obj = CAR(list);
    if(obj->type == listt && CAR(obj) == nil_obj && CDR(obj) == nil_obj) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

Object *la_list(Object *list) {
    Object *ret = NULL;
    Object *retb = ret;
    while(list != nil_obj) {
        retb = list_append(retb, CAR(list));
        if(ret == NULL) {
            ret = retb;
        }
        list = CDR(list);
    }
    return ret;
}

Object *la_car(Object *list) {
    if(CAR(list) != nil_obj && CAR(list)->type == listt) {
        return CAR(CAR(list));
    } else {
        return err_init("type error in car\n");
    }
}

Object *la_cdr(Object *list) {
    if(CAR(list) != nil_obj && CAR(list)->type == listt) {
        return CDR(CAR(list));
    } else {
        return err_init("type error in cdr\n");
    }
}

Object *la_cons(Object *list) {
    Object *arg2 = CAR(CDR(list));
    Object *ret = cell_init();
    CAR(ret) = CAR(list);
    // cons to an empty list
    if(arg2->type == listt && CAR(arg2) == nil_obj && CDR(arg2) == nil_obj) {
        return ret;
    } else {
        // CDR(ret) = CAR(CDR(list));
        CDR(ret) = arg2;
        return ret;
    }
}

Object *la_reverse_mut(Object *list) {
    Object *prev = nil_obj;
    Object *next = nil_obj;
    Object *curr = CAR(list);

    while(curr != nil_obj) {
        next = CDR(curr);
        CDR(curr) = prev;
        prev = curr;
        curr = next;
    }
    return prev;
}

/*
 * HASHMAP FUNCTIONS
 *
 */

Object *la_hashmap_init(Object *list) {
    Object *size = CAR(list);
    if(size->type == intt) {
        return obj_hashmap_init(size->data.int_val);
    } else {
        return err_init("map init expects int size");
    }
}

Object *la_hashmap_get(Object *list) {
    Object *hashmap = CAR(list);
    Object *key = CAR(CDR(list));
    Object *res = hashmap_get(hashmap->data.hashmap, key);

    Object *ret = cell_init();
    CDR(ret) = cell_init();
    if(res == NULL) {
        CAR(ret) = NIL;
        CAR(CDR(ret)) = NIL;
    } else {
        CAR(ret) = res;
        CAR(CDR(ret)) = tru_obj;
    }
    return ret;
}

Object *la_hashmap_set(Object *list) {
    Object *hashmap = CAR(list);
    Object *key = CAR(CDR(list));
    Object *val = CAR(CDR(CDR(list)));
    hashmap_set(hashmap->data.hashmap, key, val);
    return hashmap;
}

/*
 * STRING FUNCTIONS
 *
 */

Object *la_to_string(Object *list) {
    Object *obj = CAR(list);
    if(obj->type == symt  || obj->type == keywordt) {
        return obj_init_str(strt, obj_string(obj));
    } else if(obj->type == strt) {
        return obj;
    } else {
        printf("error: can't make %s into string\n", type_to_str(obj->type));
        return nil_obj;
    }
}

Object *la_char_at(Object *list) {
    Object *str = CAR(list);
    Object *idx = CAR(CDR(list));
    if(str->type != strt) {
        printf("error: can't take char at from %s type\n",
            type_to_str(str->type));
        return err_init("type error");
    } else if(idx->type != intt) {
        printf("error: char at index must be int, not %s type\n",
            type_to_str(idx->type));
        return err_init("type error");
    } else {
        if(idx->data.int_val < 0) {
            return nil_obj;
        }
        char *s = obj_string(str);
        for(int i = 0; i <= idx->data.int_val; i ++) {
            if(s[i] == '\0') {
                return nil_obj;
            }
        }
        return obj_init_str_len(strt, s + idx->data.int_val, 1);
    }
}

/*
Object *la_str_len(Object *list) {
    Object *obj = CAR(list);
    if(obj->type == strt) {
        int len = strlen(obj_string(obj));
        union Data dat = { .int_val = len };
        return obj_init(intt, dat);
    } else {
        return err_init("type error");
    }
}
*/

Object *la_str_cat(Object *list) {
    char *buff = malloc(16);
    int len = 0;
    int capacity = 16;
    while(list != nil_obj) {
        Object *s = CAR(list);
        if(s->type != strt) {
            free(buff);
            printf("str cat expects string type, not %s\n", type_to_str(s->type));
            return err_init("type error");
        }
        char *str = obj_string(s);
        while(*str != '\0') {
            buff[len++] = *str;
            str ++;
            if(len == capacity - 2) { // zero index + null terminator
                capacity *= 2;
                buff = realloc(buff, capacity);
            }
        }
        list = CDR(list);
    }
    buff[len] = '\0';
    Object *ret = obj_init_str_len(strt, buff, len);
    free(buff);
    return ret;
}

Object *la_print(Object *list) {
    obj_print(CAR(list), 0);
    printf("\n");
    return nil_obj;
}

Object *la_pprint(Object *list) {
    obj_print(CAR(list), 1);
    printf("\n");
    return nil_obj;
}

Object *la_debug(Object *list) {
    obj_debug(CAR(list));
    return nil_obj;
}

void insert_function(char *name, Object *(fn_ptr)(Object *)) {
    union Data dat = { .fn_ptr = fn_ptr };
    Object *fn = obj_init(natfnt, dat);

    Object *sym = obj_init_str(symt, name);
    envir_set(curr_envir, sym, fn);
}

void lang_init() {
    union Data dat = { .int_val = 0 };
    tru_obj = obj_init(intt, dat);
    envir_set_str(curr_envir, "t", tru_obj);

    envir_set_str(curr_envir, "nil", nil_obj);

    insert_function("+", la_sum);
    insert_function("<", la_lt);
    insert_function("=", la_eq);

    insert_function("nil?", la_is_nil);
    insert_function("empty?", la_is_empty);

    // list functions
    insert_function("list", la_list);
    insert_function("car", la_car);
    insert_function("cdr", la_cdr);
    insert_function("cons", la_cons);
    insert_function("reverse!", la_reverse_mut);

    // map functions
    insert_function("make-hashmap", la_hashmap_init);
    insert_function("hashmap-get", la_hashmap_get);
    insert_function("hashmap-set!", la_hashmap_set);

    // string functions
    insert_function("to-string", la_to_string);
    insert_function("char-at", la_char_at);
    // insert_function("str-len", la_str_len);
    insert_function("str-cat", la_str_cat);

    insert_function("print", la_print);
    insert_function("pprint", la_pprint);
    insert_function("debug", la_debug);
}
