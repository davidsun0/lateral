#include <stdlib.h>
#include <stdio.h>

#include "eval.h"
#include "reader.h"

#include "core.h"

Object *sum(List *list) {
    int sum = 0;
    while(list != NULL) {
        Object *obj = list->obj;
        if(obj->type == intt) {
            sum += obj->data.int_val;
        } else {
            // error
        }
        list = list->next;
    }
    union Data dat = { .int_val = sum };
    return obj_init(intt, dat);
}

void insert_function(char *name, Object *(fn_ptr)(List *)) {
    union Data dat = { .fn_ptr = fn_ptr };
    Object *fn = obj_init(natfnt, dat);
    // dat = { .symt = name };
    dat.ptr = la_strdup(name);
    Object *sym = obj_init(symt, dat);
    envir_set(curr_envir, sym, fn);
}

void lang_init() {
    insert_function("+", sum);
}
