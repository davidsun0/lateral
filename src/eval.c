#include <stdlib.h>
#include <stdio.h>

#include "lang.h"
#include "list.h"
#include "env.h"
#include "error.h"

#include "eval.h"

struct StackFrame* stack;
struct Envir* curr_env;

/**
 * Pushes an S expression to the stack and sets the evaluation mode.
 * A new stack frame is allocated and must be deallocated with stack_pop.
 *
 * @param ast       the abstract syntax tree to be evaluated
 * @param exe_mode  how the ast will be evaluated (i.e. apply, eval)
 */
static void stack_push(struct Object* ast, int exe_mode) {
    struct StackFrame* new_frame = malloc(sizeof(struct StackFrame));
    new_frame->prev = stack;

    new_frame->fn = NULL;
    new_frame->expr = ast;
    new_frame->working = NULL;
    new_frame->ret = NULL;
    
    new_frame->exe_mode = exe_mode;
    // for debugging
    new_frame->eval_index = -1;

    stack = new_frame;
}

/**
 * Pops the current stack frame and passes on the current return value.
 * Frees the stack frame.
 */
static void stack_pop() {
    if(stack == NULL) {
        printf("error: attempting to pop null stack\n");
        return;
    }

    if(stack->prev != NULL) {
        stack->prev->ret = stack->ret;
    }
    struct StackFrame* old_frame = stack;
    stack = stack->prev;
    free(old_frame);
}

/**
 * Destroys the entire program stack.
 * Used only for debugging.
 * Causes undefined behavior.
 */
static void stack_destroy() {
    while(stack->prev != NULL) {
        stack_pop();
    }
    stack->expr = NULL;
}

/**
 * Prints debug information for all stack frames and all pointers on the stack.
 */
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

/**
 * Non destructively creates an enironment and pushes it to the enivronment
 * list. Does not perform any error checking.
 *
 * @param syms  list of symbols to bind
 * @param vals  list of matching values for the symbols
 */
static void envir_push_bindings(struct List* syms, struct List* vals) {
    int size = list_bare_length(syms);
    struct Envir* local = envir_init(size * 2);
    local->outer = curr_env;
    curr_env->inner = local;

    for(int i = 0; i < size; i ++) {
        envir_set(local, syms->obj->data.ptr, vals->obj);
        syms = syms->next;
        vals = vals->next;
    }

    curr_env = local;
}

/**
 * Non destructively creates an environment and pushes it to the environment
 * list. Does not perform any error checking.
 *
 * @param binds list of alternating symbols and values to insert into the new
 * enrironment
 */
static void envir_push_binding_list(struct List* binds) {
    int size = list_bare_length(binds) / 2;
    struct Envir* local = envir_init(size * 2);
    local->outer = curr_env;
    curr_env->inner = local;

    for(int i = 0; i < size; i ++) {
        envir_set(local, binds->obj->data.ptr, binds->next->obj);
        binds = binds->next->next;
    }

    curr_env = local;
}

/**
 * Sets the current environment to curr_env->outer and frees the old
 * environment.
 */
static void envir_pop() {
    struct Envir* old = curr_env;
    curr_env = old->outer;
    curr_env->inner = NULL;

    envir_free(old);
}

/**
 * Evaluates special forms on the stack.
 *
 * @return  0 if the stack's current expression is not a special form or if an
 * error occurs. 1 if otherwise, skipping the effects of apply()
 */
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
        // push object to environment
        // TODO: error checking on args
        if(stack->ret == NULL) {
            stack_push(expr->next->obj, apply_exe);
        } else {
            struct Object* sym = expr->obj;
            struct Object* val = stack->ret;
            envir_set(curr_env, sym->data.ptr, val);
            stack_pop();
        }
    } else if(object_equals_symbol(form, "fn") ||
            object_equals_symbol(form, "macro")) {
        // create a fn/macro object with the given args / ast
        struct Object* output;
        if(object_equals_symbol(form, "fn")) {
            output = object_init_type(func_type);
        } else {
            output = object_init_type(macro_type);
        }
        stack->ret = output;
        struct Func* fn = malloc(sizeof(struct Func));
        output->data.ptr = fn;
        // TODO: error checking on args
        fn->args = object_copy(expr->obj);
        fn->expr = object_copy(expr->next->obj);
        stack_pop();
    } else if(object_equals_symbol(form, "if")) {
        if(stack->ret == NULL) {
            stack_push(expr->obj, apply_exe);
        } else {
            // TODO: error checking on args
            if(stack->ret != nil_obj) {
                stack->expr = expr->next->obj;
            } else {
                stack->expr = expr->next->next->obj;
            }
            stack->exe_mode = apply_exe;
            stack->ret = NULL;
        }
    } else if(object_equals_symbol(form, "let")) {
        // push bindings
        // evaludate body
        if(stack->ret == NULL) {
            // error checking
            if(expr->obj->type != list_type) {
                printf("let expects a list of bindings\n");
                stack->ret = error_init_bare();
                return 0;
            } else if(list_length(expr->obj) % 2 != 0) {
                printf("let expects an even number of bindings\n");
                stack->ret = error_init_bare();
                return 0;
            } else if(expr->next->next != NULL) {
                printf("let expects only one body expression\n");
                stack->ret = error_init_bare();
                return 0;
            }
            // TODO: check that bindings are symbols
            envir_push_binding_list(expr->obj->data.ptr);
            // object_debug(expr->next->obj);
            stack_push(expr->next->obj, apply_exe);
        } else {
            // pop bindings
            envir_pop();
            stack_pop();
        }
    } else if(object_equals_symbol(form, "loop")) {
        // store loop body on stack
    } else if(object_equals_symbol(form, "recur")) {
        // scan stack until loop or result frame
        // rebind and execute loop body
    } else if(object_equals_symbol(form, "quasiquote")) {

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
            stack_push(obj, apply_exe);
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
            stack_push(((struct Func*) stack->fn->data.ptr)->expr, eval_exe);
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
            stack_push(obj, apply_exe);
        }
    }
}

static void apply() {
    if(special_form()) {
        return;
    }

    if(stack->ret == NULL) {
        stack_push(stack->expr, eval_exe);
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
                // TODO: check number of arguments
                // check for &rest and construct list
                struct List* syms = ((struct Func*) func->data.ptr)->args->data.ptr;
                struct List* vals = args->next;
                envir_push_bindings(syms, vals);
                stack_push(((struct Func*) func->data.ptr)->expr, eval_exe);
            } else {
                stack->ret = stack->working;
                envir_pop();
                stack_pop();
            }
        } else {
            printf("error: symbol ");
            object_print_string(func);
            printf(" is not a function\n");
            stack->ret = error_init(type_err, "symbol is not a function");
        }
    }
}

struct Object* lat_evaluate(struct Envir* envir, struct Object* ast) {
    curr_env = envir;
    // stack frame to collect the result
    stack_push(NULL, result_exe);
    // stack frame with starting expression
    stack_push(ast, apply_exe);
    while(!(stack->exe_mode == result_exe && stack->ret != NULL)) {
        if(stack->expr->type == symbol) {
            if(stack->ret != NULL) {
                printf("warning: stack result is non null while evaluating sym\n");
            }
            stack->ret = envir_search(curr_env, stack->expr->data.ptr);
            if(stack->ret == NULL) {
                stack->ret = error_init(name_err, "symbol not found in environment");
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
