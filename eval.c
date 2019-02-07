#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "object.h"
#include "list.h"
#include "hash.h"
#include "env.h"

#include "eval.h"

struct Object* fn_eval(struct List* args) {
    return NULL;
}

struct Object* lambda(struct List* args) {
    if(args->obj->type != list_type 
            && args->next->next != NULL) {
        // TODO: error checking
        printf("wrong number of arguments to fn\n");
        return NULL;
    }
    struct Func* fn = malloc(sizeof(struct Func));
    fn->args = args->obj->data.ptr;
    fn->expr = (struct Object*) (args->next->obj);
    // list_print(fn->args);
    // object_print_debug(fn->expr);
    union Data data;
    data.func = fn;
    return object_init(func_type, data);
}

struct Object* eval_eval(struct Envir* env, struct Object* obj) {
    if(obj->type == list_type) {
        struct List* list = obj->data.ptr;
        // call eval on every member, returning new list
        struct Object* output = list_init();

        struct Object* elem;
        while(list != NULL) {
            elem = eval_apply(env, list->obj);
            list_append_object(output, elem);
            list = list->next;
        }
        return output;
     } else if(obj->type == symbol) {
        // look up symbol in environment
        struct Object* val = envir_search(env, obj->data.ptr);
        if(val == NULL) {
            // TODO: error handling
            printf("error: symbol %s not found\n", (char*) obj->data.ptr);
            return NULL;
        } else {
            return val;
        }
     } else {
        // objects evaluate to themselves
        return obj;
     }
}

struct Object* eval_apply(struct Envir* env, struct Object* obj) {
    if(obj->type == list_type) {
        // special forms
        if(obj->type == list_type) {
            struct List* sp = obj->data.ptr;
            if(object_equals_symbol(sp->obj, "quote")) {
                // TODO: copy object?
                return sp->next->obj;
            } else if(object_equals_symbol(sp->obj, "fn")) {
                return lambda(sp->next);
            }
        }
        struct Object* list = eval_eval(env, obj);
        
        // for special forms like quote
        if(list->type != list_type) {
            return list;
        }

        struct List* arg_list = (struct List*)list->data.ptr;
        struct Object* func = arg_list->obj;
        arg_list = arg_list->next;

        if(func->type == c_fn) {
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
            struct Object* result = eval_apply(local, fn_struct->expr);
            // clean up temporary environment
            envir_free(local);
            return result;
        } else {
            printf("error: symbol ");
            object_print_string(func);
            printf(" is not a function\n");
            return NULL;
        }
    } else {
        return eval_eval(env, obj);
    }
}
