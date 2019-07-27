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
    if(CAR(list)->type == listt && CAR(CAR(list)) == nil_obj) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

Object *la_list(Object *list) {
    Object *ret = cell_init();
    Object *retb = ret;
    while(list != nil_obj) {
        retb = list_append(retb, CAR(list));
        list = CDR(list);
    }
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
    dat.ptr = la_strdup(name);
    Object *sym = obj_init(symt, dat);
    envir_set(curr_envir, sym, fn);
}

void lang_init() {
    // printf("size of object: %ld\n", sizeof(Object));

    union Data dat = { .int_val = 0 };
    tru_obj = obj_init(intt, dat);
    envir_set_str(curr_envir, "t", tru_obj);

    // NIL is a list whose CAR and CDR are NIL
    /*
    union Data ndat = { .cell = {NULL, NULL}};
    nil_obj = obj_init(listt, ndat);
    CAR(nil_obj) = nil_obj;
    CDR(nil_obj) = nil_obj;
    */
    envir_set_str(curr_envir, "nil", nil_obj);

    insert_function("+", la_sum);
    insert_function("nil?", la_is_nil);
    insert_function("list", la_list);
    insert_function("print", la_print);
    insert_function("pprint", la_pprint);
    insert_function("debug", la_debug);
}
