#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"
#include "hash.h"
#include "reader.h"
#include "env.h"
#include "eval.h"

struct Envir* global_env;
struct Envir* user_env;

struct Object* true_obj;
struct Object* nil_obj;

struct Object* lat_cons(struct List* args) {
    if(list_length(args) != 2
            || args->next->obj == NULL
            || args->next->obj->type != list_type) {
        printf("error: cons expects two arguments\n");
        return NULL;
    }
    struct Object* output = list_init();
    struct Object* copy = object_copy(args->obj);
    list_append_object(output, copy);
    struct List* list = args->next->obj->data.ptr;
    while(list != NULL) {
        if(list->obj != NULL) {
            copy = object_copy(list->obj);
            list_append_object(output, copy);
        }
        list = list->next;
    }
    return output;
}

struct Object* lat_first(struct List* args) {
    if(args->obj == NULL || args->next != NULL) {
        printf("error in 'first'\n");
        return NULL;
    }
    struct Object* obj = args->obj;
    if(obj->type == list_type) {
        struct List* list = obj->data.ptr;
        return list->obj;
    } else {
        return nil_obj;
    }
}

struct Object* lat_rest(struct List* args) {
    if(args->obj == NULL || args->next != NULL) {
        printf("error in 'rest'\n");
        return NULL;
    }
    struct Object* obj = args->obj;
    if(obj->type == list_type) {
        struct List* list = obj->data.ptr;
        list = list->next;
        struct Object* output = list_init();
        while(list != NULL) {
            struct Object* copy = object_copy(list->obj);
            list_append_object(output, copy);
            list = list->next;
        }
        return output;
    } else {
        return nil_obj;
    }
}

struct Object* lat_concat(struct List* args) {
    struct Object* output = list_init();
    while(args != NULL) {
        if(args->obj != NULL && args->obj->type == list_type) {
            struct List* list = args->obj->data.ptr;
            while(list != NULL) {
                struct Object* copy = object_copy(list->obj);
                list_append_object(output, copy);
                list = list->next;
            }
        } else {
            struct Object* copy = object_copy(args->obj);
            list_append_object(output, copy);
        }
        args = args->next;
    }
    return output;
}

struct Object* lat_list(struct List* args) {
    struct Object* output = list_init();
    while(args != NULL) {
        struct Object* copy = object_copy(args->obj);
        list_append_object(output, copy);
        args = args->next;
    }
    return output;
}

struct Object* lat_equals(struct List* args) {
    if(args == NULL || args->obj == NULL) {
        return nil_obj;
    } else {
        struct Object* first = args->obj;
        struct List* node = args->next;
        while(node != NULL) {
            if(object_equals_value(first, node->obj)) {
                node = node->next;
            } else {
                return nil_obj;
            }
        }
        return true_obj;
    }
}

struct Object* lat_read(struct List* args) {
    if(args->obj == NULL || args->obj->type != string || args->next != NULL) {
        printf("error: read expects one string argument\n");
        return NULL;
    }
    struct Object* output = read_string(args->obj->data.ptr);
    if(output == NULL) {
        return nil_obj;
    } else {
        return output;
    }
}

struct Object* lat_eval(struct List* args) {
    if(args->obj == NULL || args->next != NULL) {
        printf("error: eval expects one argument\n");
        return NULL;
    }
    struct Object* output = eval_apply(user_env, args->obj);
    return output;
}

struct Object* lat_print(struct List* args) {
    while(args != NULL) {
        object_print_string(args->obj);
        args = args->next;
        if(args != NULL) {
            printf(" ");
        }
    }
    printf("\n");
    return nil_obj;
}

struct Object* lat_plus(struct List* args) {
    if(args->obj == NULL) {
        printf("error: wrong number of arguments to +\n");
        // TODO: error checking
        return NULL;
    }
    int value = 0;
    while(args != NULL) {
        if(args->obj != NULL && args->obj->type != int_type) {
            printf("error: wrong type of argument to +, expected int\n");
            return NULL;
        }
        value += args->obj->data.int_type;
        args = args->next;
    }
    union Data data;
    data.int_type = value;
    return object_init(int_type, data);
}
void envir_insert_cfn(struct Object* (*fn_ptr)(struct List*), char* name) {
    union Data data;
    data.fn_ptr = fn_ptr;
    struct Object* fn = object_init(c_fn, data);
    envir_set(global_env, name, fn);
}

void env_init() {
    global_env = envir_init(128);

    union Data temp;
    temp.ptr = NULL;
    true_obj = object_init(true, temp);
    nil_obj = object_init(nil, temp);
    envir_set(global_env, "t", true_obj);
    envir_set(global_env, "nil", nil_obj);

    envir_insert_cfn(&lat_cons, "cons");
    envir_insert_cfn(&lat_concat, "concat");
    envir_insert_cfn(&lat_list, "list");

    envir_insert_cfn(&lat_read, "read");
    envir_insert_cfn(&lat_eval, "eval");
    envir_insert_cfn(&lat_print, "print");

    envir_insert_cfn(&lat_plus, "+");
    envir_insert_cfn(&lat_equals, "=");

    struct Object* tree = read_module("./core.lisp");
    struct List* exprs = tree->data.ptr;
    while(exprs != NULL) {
        // object_debug(exprs->obj);
        object_print_string(exprs->obj);
        printf("\n");
        eval_expression(global_env, exprs->obj);
        exprs = exprs->next;
    }

    user_env = envir_init(128);
    user_env->outer = global_env;
    global_env->inner = user_env;
}
