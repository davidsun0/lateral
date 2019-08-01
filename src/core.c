#include <stdlib.h>
#include <stdio.h>

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
    insert_function("nil?", la_is_nil);
    insert_function("empty?", la_is_empty);
    insert_function("list", la_list);
    insert_function("car", la_car);
    insert_function("cdr", la_cdr);
    insert_function("cons", la_cons);
    insert_function("reverse!", la_reverse_mut);

    insert_function("print", la_print);
    insert_function("pprint", la_pprint);
    insert_function("debug", la_debug);
}
