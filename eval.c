#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "object.h"
#include "list.h"
#include "hash.h"
#include "env.h"
#include "lang.h"

#include "eval.h"

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
        struct List* sp = obj->data.ptr;
        // TODO: compare all special forms at once
        if(sp->obj == NULL) {
            // empty list evaluates to nil
            return nil_obj;
        } else if(object_equals_symbol(sp->obj, "quote")) {
            // TODO: copy object?
            return sp->next->obj;
        } else if(object_equals_symbol(sp->obj, "or")) {
            sp = sp->next;
            if(sp == NULL) {
                printf("error: or expects at least one argument\n");
                return NULL;
            }
            struct Object* val;
            while(sp != NULL) {
                val = eval_apply(env, sp->obj);
                if(val == NULL)
                    return NULL;
                if(val != nil_obj)
                    return true_obj;
                sp = sp->next;
            }
            return nil_obj;
        } else if(object_equals_symbol(sp->obj, "and")) {
            sp = sp->next;
            if(sp == NULL) {
                printf("error: and expects at least one argument\n");
                return NULL;
            }
            struct Object* val;
            while(sp != NULL) {
                val = eval_apply(env, sp->obj);
                if(val == NULL)
                    return NULL;
                if(val == nil_obj)
                    return nil_obj;
                sp = sp->next;
            }
            return true_obj;
        } else if(object_equals_symbol(sp->obj, "fn")) {
            return lambda(sp->next);
        } else if(object_equals_symbol(sp->obj, "def")) {
            if(list_length(sp) != 3 || sp->next->obj->type != symbol) {
                printf("error: wrong type / number of args to def\n");
                return NULL;
            }
            struct Object* sym = sp->next->obj;
            struct Object* val = sp->next->next->obj;
            val = eval_apply(env, val);
            envir_set(env, sym->data.ptr, val);
            return val;
        } else if(object_equals_symbol(sp->obj, "if")) {
            int argc = list_length(sp) - 1;
            if(argc == 2 || argc == 3) {
                struct Object* pred = sp->next->obj;
                struct Object* tclause = sp->next->next->obj;
                if(!object_equals_value(nil_obj, eval_apply(env, pred))) {
                    return eval_eval(env, tclause);
                } else {
                    if(argc == 3) {
                        struct Object* fclause = sp->next->next->next->obj;
                        return eval_eval(env, fclause);
                    } else {
                        return nil_obj;
                    }
                }
            } else {
                printf("error: wrong number of args to if (expects 2 or 3)\n");
                return NULL;
            }
        } else if(object_equals_symbol(sp->obj, "cond")) {
            sp = sp->next;
            int argc = list_length(sp);
            if(argc == 0 || argc % 2 != 0) {
                printf("error: cond expects an even number of arguments\n");
                return NULL;
            }
            struct Object* pred;
            int argnum = 0;
            while(sp != NULL) {
                if(object_equals_symbol(sp->obj, ":else")) {
                    if(argnum != argc - 2) {
                        printf("%d / %d\n", argnum, argc);
                        printf("unexpected :else symbol\n");
                        return NULL;
                    } else {
                        return eval_apply(env, sp->next->obj);
                    }
                }
                pred = eval_apply(env, sp->obj);
                if(pred == NULL)
                    return NULL;
                if(pred != nil_obj)
                    return eval_apply(env, sp->next->obj);
                argnum += 2;
                sp = sp->next->next;
            }
            return nil_obj;
        } else if(object_equals_symbol(sp->obj, "let")) {
            sp = sp->next;
            // get bindings
            if(sp->obj->type != list_type 
                    || list_length(sp->obj->data.ptr) % 2 != 0) {
                printf("error: let expects an even number of bindings\n");
                return NULL;
            }
            struct List* bindings = sp->obj->data.ptr;
            struct Envir* local = envir_init(list_length(bindings) * 2);
            // map bindings to environment
            while(bindings != NULL) {
                if(bindings->obj->type != symbol) {
                    printf("error: binding is not symbol\n");
                    envir_free(local);
                    return NULL;
                }
                envir_set(local, (char*) bindings->obj->data.ptr,
                        bindings->next->obj);
                bindings = bindings->next->next;
            }
            // push environment
            local->outer = env;
            // evaluate like progn
            sp = sp->next;
            while(sp->next != NULL) {
                eval_apply(local, sp->obj);
                sp = sp->next;
            }
            struct Object* result = eval_apply(local, sp->obj);
            envir_free(local);
            return result;
        } else if(object_equals_symbol(sp->obj, "progn")) {
            sp = sp->next;
            while(sp->next != NULL) {
                eval_apply(env, sp->obj);
                sp = sp->next;
            }
            return eval_apply(env, sp->obj);
        }


        struct Object* list = eval_eval(env, obj);
        // if the expression evaluated to non-list type
        // i.e. evaluated by a special form
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
