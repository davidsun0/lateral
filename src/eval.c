#include <stdlib.h>
#include <stdio.h>

#include "lang.h"
#include "list.h"
#include "env.h"
#include "error.h"

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
    new_frame->exe_mode = unknown;
    new_frame->eval_index = -1;

    stack = new_frame;
}

static void stack_pop() {
    if(stack == NULL) {
        printf("warning: attempting to pop null stack\n");
        return;
    }

    if(stack->prev != NULL) {
        stack->prev->ret = stack->ret;
    }
    struct StackFrame* old_frame = stack;
    stack = stack->prev;
    free(old_frame);
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
        printf("exe:  %d\n", frame->exe_mode);
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
    struct Envir* local = envir_init(size * 2);
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
    // printf("evaluating special form\n");
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
    if (object_equals_symbol(form, "def")) {
        if(stack->ret == NULL) {
            stack_push(expr->next->obj);
            stack->exe_mode = apply_exe;
        } else {
            struct Object* sym = expr->obj;
            struct Object* val = stack->ret;
            envir_set(curr_env, sym->data.ptr, val);
            stack_pop();
        }
    } else if(object_equals_symbol(form, "fn") ||
            object_equals_symbol(form, "macro")) {
        struct Object* output;
        if(object_equals_symbol(form, "fn")) {
            output = object_init_type(func_type);
        } else {
            output = object_init_type(macro_type);
        }
        stack->ret = output;
        struct Func* fn = malloc(sizeof(struct Func));
        output->data.ptr = fn;
        fn->args = object_copy(expr->obj);
        fn->expr = object_copy(expr->next->obj);
        stack_pop();
    } else if(object_equals_symbol(form, "if")) {
        if(stack->ret == NULL) {
            stack_push(expr->obj);
            stack->exe_mode = apply_exe;
        } else {
            if(stack->ret != nil_obj) {
                stack->expr = expr->next->obj;
            } else {
                stack->expr = expr->next->next->obj;
            }
            stack->exe_mode = apply_exe;
            stack->ret = NULL;
        }
    } else {
        return 0;
    }
    return 1;
}

static void eval() {
    if(stack->working == NULL) {
        if(list_is_empty(stack->expr->data.ptr)) {
            // empty list evaluates to nil
            stack->ret = nil_obj;
            stack_pop();
            // pop underlying apply stack frame
            stack_pop();
        } else {
            // create a evaluated version of stack->expr
            stack->working = list_init();
            stack->eval_index = 1;
            // push first item in the list
            struct Object* obj = list_get(stack->expr, 0);
            stack_push(obj);
            stack->exe_mode = apply_exe;
        }
    } else {
        if(stack->eval_index == 1) {
            stack->fn = stack->ret;
        }

        if(stack->fn != NULL && stack->fn->type == macro_type) {
            struct List* syms = ((struct Func*) stack->ret->data.ptr)->args->data.ptr;
            struct List* vals = ((struct List*) stack->expr->data.ptr)->next;
            envir_push_bindings(syms, vals);
            stack->exe_mode = macro_exe;
            stack_push(((struct Func*) stack->fn->data.ptr)->expr);
            stack->exe_mode = eval_exe;
            return;
        }
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
            stack->exe_mode = apply_exe;
        }
    }
}

static void apply() {
    if(special_form()) {
        return;
    }

    if(stack->ret == NULL) {
        stack_push(stack->expr);
        stack->exe_mode = eval_exe;
    } else {
        if(stack->ret->type != list_type) {
            printf("warning: apply expected list type, but got ");
            object_print_type(stack->ret->type);
            printf("\n");
            stack_pop();
            return;
        }

        struct List* args = (struct List*) stack->ret->data.ptr;
        struct Object* func = args->obj;

        if(func->type == c_fn) {
            stack->ret = func->data.fn_ptr(args->next);
            stack_pop();
        } else if(func->type == func_type) {
            if(stack->working == NULL) {
                struct List* syms = ((struct Func*) func->data.ptr)->args->data.ptr;
                struct List* vals = args->next;
                envir_push_bindings(syms, vals);
                stack_push(((struct Func*) func->data.ptr)->expr);
                stack->exe_mode = eval_exe;
            } else {
                stack->ret = stack->working;
                envir_pop();
                stack_pop();
            }
        } else {
            printf("error: symbol ");
            object_print_string(func);
            printf(" is not a function\n");
            stack->ret = error_init();
        }
    }
}

struct Object* lat_evaluate(struct Envir* envir, struct Object* ast) {
    curr_env = envir;
    // stack frame to collect the result
    stack_push(NULL);
    stack->exe_mode = result_exe;
    // stack frame with starting expression
    stack_push(ast);
    stack->exe_mode = apply_exe;
    while(!(stack->exe_mode == result_exe && stack->ret != NULL)) {
        if(stack->expr->type == symbol) {
            if(stack->ret != NULL) {
                printf("warning: stack result is non null while evaluating sym\n");
            }
            stack->ret = envir_search(curr_env, stack->expr->data.ptr);
            if(stack->ret == NULL) {
                stack->ret = error_init();
            }
            stack_pop();
        } else if(stack->expr->type != list_type) {
            if(stack->ret != NULL) {
                printf("warning: stack result is non null while evaluating obj\n");
            }
            stack->ret = stack->expr;
            stack_pop();
        } else {
            if(stack->exe_mode == eval_exe) {
                eval();
            } else if(stack->exe_mode == apply_exe) {
                apply();
            } else if(stack->exe_mode == macro_exe) {
                // macro execution begins in eval
                // pop macro's temporary environment
                envir_pop();
                // pop eval turned macro stack frame
                stack_pop();
                // pop underlying apply stack frame
                stack_pop();
            } else {
                printf("fatal: unknown execution mode %d\n", stack->exe_mode);
                stack_print();
                stack_destroy();
                break;
            }
        }
    }
    struct Object* output = stack->ret;
    stack_pop();
    return output;
}
