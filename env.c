#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "hash.h"

#include "env.h"

struct HashMap* envir;

struct Object* add(struct Object* a, struct Object* b) {
    if(a == NULL || b == NULL) {
        return NULL;
    } else if(a->type == integer && b->type == integer){
        struct Object* obj = malloc(sizeof(struct Object));
        obj->type = integer;
        obj->data.integer = a->data.integer + b->data.integer;
        return obj;
    } else {
        // TODO: some kind of error checking
        return NULL;
    }
}

void env_init() {
    envir = hashmap_init(128);
    struct Object* plus = malloc(sizeof(struct Object));
    plus->type = c_fn;
    plus->data.fn_ptr = &add;
    hashmap_set(envir, "+", plus);

    struct Object* test = hashmap_get(envir, "+");
    printf("%p : %p\n", (void*) plus, (void*) test);
}
