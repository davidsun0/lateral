#include <stdlib.h>

#include "object.h"
#include "hash.h"
#include "env.h"

struct Envir* global_env;

struct Object* sum(struct Object* a, struct Object* b) {
    if(a == NULL || b == NULL) {
        return NULL;
    } else if(a->type == int_type && b->type == int_type){
        union Data data;
        data.int_type = a->data.int_type + b->data.int_type;
        return object_init(int_type, data);
    } else {
        // TODO: some kind of error checking
        return NULL;
    }
}

void env_init() {
    global_env = envir_init(128);

    union Data data;
    data.fn_ptr = &sum;
    struct Object* plus = object_init(c_fn, data);
    envir_set(global_env, "+", plus);
}
