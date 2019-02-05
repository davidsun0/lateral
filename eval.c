#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"
#include "hash.h"

#include "eval.h"

struct Object* eval_eval(struct HashMap* env, struct Object* obj) {
    if(obj->type == symbol) {
        // look up symbol in environment
        struct Object* val = hashmap_get(env, obj->data.ptr);
        if(val == NULL) {
            // TODO: error handling
            printf("error: symbol %s not found\n", (char*) obj->data.ptr);
            return NULL;
        } else {
            return val;
        }
     } else if(obj->type == list_type) {
        // call eval on every member, returning new list
        struct Object* output = list_init();

        struct List* list = obj->data.ptr;
        struct Object* elem;
        while(list != NULL) {
            elem = eval_apply(env, list->obj);
            list_append_object(output, elem);
            list = list->next;
        }
        return output;
     } else {
        // objects evaluate to themselves
        return obj;
     }
}

struct Object* eval_apply(struct HashMap* env, struct Object* obj) {
    if(obj->type == list_type) {
        struct Object* list = eval_eval(env, obj);
        struct List* args = (struct List*)list->data.ptr;
        struct Object* func = args->obj;
        args = args->next;
        // somehow call func on args
        if(func->type == c_fn) {
            struct Object* arg1 = NULL;
            if(args != NULL) {
                arg1 = args->obj;
            }
            struct Object* arg2 = NULL;
            if(args != NULL && args->next != NULL) {
                arg2 = args->next->obj;
            }
            return func->data.fn_ptr(arg1, arg2);
        } else {
            return NULL;
        }
    } else {
        return eval_eval(env, obj);
    }
}
