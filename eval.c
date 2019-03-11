#include <stdlib.h>
#include <stdio.h>

#include "lang.h"
#include "list.h"

#include "eval.h"

struct StackFrame* stack;
struct Envir* curr_env;

static void stack_push(struct Object* ast) {
    struct StackFrame* new_frame = malloc(sizeof(struct StackFrame));
    new_frame->prev = stack;

    new_frame->fn = NULL;
    new_frame->expr = ast;
    new_frame->working = NULL;
    new_frame->ret = NULL;
    
    // for debugging
    new_frame->exe_mode = -1;
    new_frame->eval_index = -1;

    stack = new_frame;
}

static void stack_pop() {
    if(stack == NULL) {
        printf("warning: attempting to pop null stack\n");
        return;
    }

    if(stack->prev == NULL) {
        stack->expr = NULL;
    } else {
        stack->prev->ret = stack->ret;
        struct StackFrame* old_frame = stack;
        stack = stack->prev;
        free(old_frame);
    }
}

static void stack_destroy() {
    while(stack->prev != NULL) {
        stack_pop();
    }
    stack->expr = NULL;
}

void stack_print() {
    struct StackFrame* frame = stack;

    printf("=== STACK ===\n");
    while(frame != NULL) {
        printf("=== STACK FRAME ===\n");
        printf("fn:   ");
        object_print_string(frame->fn);
        printf("\nexpr: ");
        object_print_string(frame->expr);
        printf("\nret:  ");
        object_print_string(frame->ret);
        printf("\nnext frame: %p\n\n", (void*) frame->prev);
        frame = frame->prev;
    }
    printf("=== END STACK ===\n\n\n");
}

static void envir_push_bindings(struct List* syms, struct List* vals) {
    int size = list_bare_length(syms);
    struct Envir* local = envir_init(size);
    local->outer = curr_env;
    curr_env = local->inner;

    for(int i = 0; i < size; i ++) {
        envir_set(local, syms->obj->data.ptr, vals->obj);
        syms = syms->next;
        vals = vals->next;
    }

    curr_env = local;
}

static void envir_pop() {
    struct Envir* old = curr_env;
    curr_env = old->outer;
    curr_env->inner = NULL;

    envir_free(old);
}

static int special_form() {
    printf("evaluating special form\n");
    struct List* expr = stack->expr->data.ptr;
    struct Object* form = expr->obj;
    expr = expr->next;

    // or, and
    // fn, macro
    // def
    // if, cond
    // let
    // do, loop / recur
    // quasiquote
    if(object_equals_symbol(form, "quote")) {
        // TODO: check arg count
        stack->expr = expr->obj;
    } else {
        return 0;
    }
    return 1;
}

static void eval() {
    if(stack->working == NULL) {
        if(list_is_empty(stack->expr->data.ptr)) {
            // empty list evaluates to itself
            stack->ret = stack->expr;
            stack_pop();
        } else {
            // create a evaluated version of stack->expr
            stack->working = list_init();
            stack->eval_index = 1;
            // push next item in the list
            struct Object* obj = list_get(stack->expr, 0);
            stack_push(obj);
        }
    } else {
        // append ret onto working
        list_append_object(stack->working, stack->ret);
        // push next object onto stack
        struct Object* obj = list_get(stack->expr, stack->eval_index);
        stack->eval_index ++;
        // if end of list, pop result
        if(obj == NULL) {
            stack->ret = stack->working;
            stack_pop();
        } else {
            stack_push(obj);
            stack->exe_mode = apply_type;
        }
    }
}

static void apply() {
    if(stack->ret == NULL) {
        // execute macros here
        if(special_form()) {
            stack->ret = stack->expr;
            stack_pop();
        } else {
            stack_push(stack->expr);
            stack->exe_mode = eval_type;
        }
    } else {
        stack->working = stack->ret;
        if(stack->working->type != list_type) {
            printf("error: apply expected list type, but got");
            object_print_type(stack->working->type);
            printf("\n");
            stack_pop();
        }

        struct List* args = (struct List*) stack->working->data.ptr;
        struct Object* func = args->obj;

        if(func->type == c_fn) {
            stack->ret = func->data.fn_ptr(args->next);
            stack_pop();
        } else {
            object_print_type(func->type);
            printf("\nimplement apply exe\n");
            object_print_string(stack->ret);
            printf("\n");
            stack_destroy();
        }
    }
}

struct Object* lat_evaluate(struct Envir* envir, struct Object* ast) {
    curr_env = envir;
    stack_push(ast);
    stack->exe_mode = apply_type;
    while(!(stack->prev == NULL && stack->expr == NULL)) {
        struct Object* expr = stack->expr;
        if(expr->type == symbol) {
            if(stack->ret != NULL) {
                printf("warning: stack result is non null while evaluating sym\n");
            }
            struct Object* obj = envir_search(curr_env, expr->data.ptr);
            stack->ret = obj;
            stack_pop();
        } else if(expr->type != list_type) {
            if(stack->ret != NULL) {
                printf("warning: stack result is non null while evaluating obj\n");
            }
            stack->ret = expr;
            stack_pop();
        } else {
            if(stack->exe_mode == eval_type) {
                eval();
            } else if(stack->exe_mode == apply_type) {
                apply();
            } else {
                printf("fatal: unknown execution mode\n");
                stack_print();
                stack_destroy();
                break;
            }
        }
    }
    struct Object* output = stack->ret;
    free(stack);
    stack = NULL;
    return output;
}
