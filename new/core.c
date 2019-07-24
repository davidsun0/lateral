#include <stdlib.h>
#include <stdio.h>

#include "eval.h"
#include "reader.h"

#include "core.h"

Object *la_sum(List *list) {
    int sum = 0;
    while(list != NULL) {
        Object *obj = list->obj;
        if(obj->type == intt) {
            sum += obj->data.int_val;
        } else {
            return err_init("type error");
        }
        list = list->next;
    }
    union Data dat = { .int_val = sum };
    return obj_init(intt, dat);
}

Object *la_is_nil(List *list) {
    if(list->obj == nil_obj) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

Object *la_list(List *list) {
    List *lista = list_init();
    List *listb = lista;
    while(list != NULL) {
        listb = list_append(listb, list->obj);
        list = list->next;
    }
    union Data dat = { .ptr = lista };
    return obj_init(listt, dat);
}

Object *la_print(List *list) {
    obj_print(list->obj, 0);
    printf("\n");
    return nil_obj;
}

Object *la_pprint(List *list) {
    obj_print(list->obj, 1);
    printf("\n");
    return nil_obj;
}

Object *la_debug(List *list) {
    obj_debug(list->obj);
    return nil_obj;
}

void insert_function(char *name, Object *(fn_ptr)(List *)) {
    union Data dat = { .fn_ptr = fn_ptr };
    Object *fn = obj_init(natfnt, dat);
    dat.ptr = la_strdup(name);
    Object *sym = obj_init(symt, dat);
    envir_set(curr_envir, sym, fn);
}

void lang_init() {
    union Data dat = { .int_val = 0 };
    tru_obj = obj_init(intt, dat);
    nil_obj = obj_init(intt, dat);

    envir_set_str(curr_envir, "t", tru_obj);
    envir_set_str(curr_envir, "nil", nil_obj);

    insert_function("+", la_sum);
    insert_function("nil?", la_is_nil);
    insert_function("list", la_list);
    insert_function("print", la_print);
    insert_function("pprint", la_pprint);
    insert_function("debug", la_debug);
}
