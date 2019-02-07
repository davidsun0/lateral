#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"
#include "hash.h"
#include "env.h"

#include "eval.h"

struct Object* eval_eval(struct Envir* env, struct Object* obj) {
    if(obj->type == symbol) {
        // look up symbol in environment
        // struct Object* val = hashmap_get(env, obj->data.ptr);
        struct Object* val = envir_search(env, obj->data.ptr);
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

struct Object* eval_apply(struct Envir* env, struct Object* obj) {
    if(obj->type == list_type) {
        struct Object* list = eval_eval(env, obj);
        struct List* arg_list = (struct List*)list->data.ptr;
        struct Object* func = arg_list->obj;
        arg_list = arg_list->next;
        // somehow call func on args
        if(func->type == c_fn) {
            /*
            struct Object* arg1 = NULL;
            if(arg_list != NULL) {
                arg1 = arg_list->obj;
            }
            struct Object* arg2 = NULL;
            if(arg_list != NULL && arg_list->next != NULL) {
                arg2 = arg_list->next->obj;
            }
            */
            return func->data.fn_ptr(arg_list);
        } else if(func->type == func_type) {
            struct Func* fn_struct = (struct Func*) func->data.ptr;
            // create local environment for function evaluation
            int argc = list_length(arg_list);
            struct Envir* local = envir_init(argc * 2);

            // map function symbols to given arguments
            struct List* arg_val = arg_list;
            struct List* arg_sym = fn_struct->args;
            while(arg_sym != NULL && arg_val != NULL) {
                if(arg_sym->obj->type != symbol) {
                    printf("error: argument is not symbol\n");
                    envir_free(local);
                    return NULL;
                }
                envir_set(local, (char*) arg_sym->obj->data.ptr, arg_val->obj);
                arg_sym = arg_sym->next;
                arg_val = arg_val->next;
            }
            if(arg_sym != NULL || arg_val != NULL) {
                printf("error: mismatched arguments\n");
                envir_free(local);
                return NULL;
            }
            // evaluate function expression
            local->outer = env;
            struct Object* result = eval_eval(local, fn_struct->expr);
            // clean up temporary environment
            envir_free(local);
            return result;
        } else {
            printf("error: token ");
            object_print_string(func);
            printf(" is not a function");
            return NULL;
        }
    } else {
        return eval_eval(env, obj);
    }
}
