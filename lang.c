#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"
#include "hash.h"
#include "env.h"

struct Envir* global_env;

struct Object* lambda(struct List* args) {
    if(args->obj->type != list_type
            || args->next->next != NULL) {
        // TODO: error checking
        printf("wrong number of arguments to fn\n");
        return NULL;
    }
    struct Func* fn = malloc(sizeof(struct Func));
    fn->args = args->obj->data.ptr;
    fn->expr = (struct Object*) (args->next->obj);
    union Data data;
    data.func = fn;
    return object_init(func_type, data);
}

struct Object* sum(struct List* args) {
    if(args->obj == NULL) {
        printf("wrong number of arguments to +\n");
        // TODO: error checking
        return NULL;
    }
    int value = 0;
    while(args != NULL) {
        if(args->obj->type != int_type) {
            printf("wrong type of argument to +, expected int\n");
            return NULL;
        }
        value += args->obj->data.int_type;
        args = args->next;
    }
    union Data data;
    data.int_type = value;
    return object_init(int_type, data);
}

void env_init() {
    global_env = envir_init(128);

    union Data data;
    data.fn_ptr = &sum;
    struct Object* plus = object_init(c_fn, data);
    envir_set(global_env, "+", plus);
}
