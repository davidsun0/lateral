#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "reader.h"
#include "hash.h"
#include "core.h"

#include "eval.h"

void stack_push(Object *ast, enum mode exe) {
    StackFrame *frame = malloc(sizeof(StackFrame));
    frame->prev = stack;
    frame->fn = NULL;
    frame->in = ast;
    frame->in_list = NULL;
    frame->out = NULL;
    frame->out_list = NULL;
    frame->ret = NULL;
    frame->exe_mode = exe;

    stack = frame;
}

void stack_pop() {
    if(stack->prev == NULL) {
        free(stack);
        stack = NULL;
    } else {
        stack->prev->ret = stack->ret;
        StackFrame *frame = stack;
        stack = stack->prev;
        free(frame);
    }
}

int envir_push(List *bindings, List *vals) {
    int size = list_length(bindings);
    int size_v = list_length(vals);
    if(size != size_v) {
        printf("error: wrong number of vals for bindings\n");
        printf("expected %d but got %d\n", size, size_v);
        return -1;
    }
    Envir *envir = envir_init(size);
    for(int i = 0; i < size; i ++) {
        envir_set(envir, bindings->obj, vals->obj);
        bindings = bindings->next;
        vals = vals->next;
    }
    envir->outer = curr_envir;
    curr_envir = envir;
    return 0;
}

void envir_pop() {
    Envir *envir = curr_envir;
    curr_envir = curr_envir->outer;
    envir_free(envir);
}

void eval() {
    Object *expr = stack->in;
    if(expr->type == symt) {
        // search for symbols in the environment
        stack->ret = envir_search(curr_envir, expr);
        if(stack->ret == NULL) {
            char *err = la_strdup("symbol not found");
            union Data dat = { .ptr = err };
            stack->ret = obj_init(errt, dat);
        }
        stack_pop();
    } else if(expr->type == listt) {
        // return (map apply expr)
        if(stack->out == NULL) {
            // initialize output list
            List *list = list_init();
            union Data dat = { .ptr = list };
            stack->out = obj_init(listt, dat);
            stack->out_list = list;
            stack->in_list = expr->data.ptr;
            stack_push(stack->in_list->obj, apply_exe);
            // advance working list pointer
            // careful! just pushed a new stack frame
            stack->prev->in_list = stack->prev->in_list->next;
        } else {
            // iterate across expr
            stack->out_list = list_append(stack->out_list, stack->ret);
            if(stack->in_list != NULL) {
                stack_push(stack->in_list->obj, apply_exe);
                stack->prev->in_list = stack->prev->in_list->next;
            } else {
                stack->ret = stack->out;
                stack_pop();
            }
        }
    } else {
        // all other types evaluate to themselves
        stack->ret = expr;
        stack_pop();
    }
}

int special_form() {
    List *list = stack->in->data.ptr;
    if(stack->in->type != listt || list->obj == NULL) {
        return 0;
    }

    if(obj_eq_sym(list->obj, "fn")) {
        list = list->next;
        if(list->obj->type != listt) {
            stack->ret = err_init("fn expects a list as its first arg\n");
            stack_pop();
            return 1;
        }
        List *arg_list = list->obj->data.ptr;
        // duplicate list into function structure
        List *args = list_init();
        List *arg0 = args;
        while(arg_list != NULL) {
            arg0 = list_append(arg0, arg_list->obj);
            arg_list = arg_list->next;
        }

        Object *expr = list->next->obj;
        struct Func func = { .args = args, .expr = expr };
        union Data dat = { .func = func };
        stack->ret = obj_init(fnt, dat);
        stack_pop();
    } else if(obj_eq_sym(list->obj, "macro")) {
        // copy and paste from fn
        list = list->next;
        if(list->obj->type != listt) {
            stack->ret = err_init("macro expects a list as its first arg\n");
            stack_pop();
            return 1;
        }
        List *arg_list = list->obj->data.ptr;
        // duplicate list into function structure
        List *args = list_init();
        List *arg0 = args;
        while(arg_list != NULL) {
            arg0 = list_append(arg0, arg_list->obj);
            arg_list = arg_list->next;
        }

        Object *expr = list->next->obj;
        struct Func func = { .args = args, .expr = expr };
        union Data dat = { .func = func };
        stack->ret = obj_init(macrot, dat);
        stack_pop();
    } else if(obj_eq_sym(list->obj, "defmacro")) {
        list = list->next;
        Object *name = list->obj;
        Object *args = list->next->obj;
        Object *expr = list->next->next->obj;
        obj_debug(name);
        obj_debug(args);
        obj_debug(expr);
        List *arg_list = list_copy(args->data.ptr);
        struct Func func = { .args = arg_list, .expr = expr };
        union Data dat = { .func = func };
        Object *macro = obj_init(macrot, dat);
        envir_set(curr_envir, name, macro);
        stack->ret = macro;
        stack_pop();
    } else if(obj_eq_sym(list->obj, "if")) {
        if(stack->out == NULL) {
            list = list->next;
            stack->in_list = list;
            stack->out = list->obj;
            stack_push(list->obj, apply_exe);
            stack->prev->in_list = stack->prev->in_list->next;
        } else {
            // the actual if statement
            if(stack->ret != nil_obj) {
                stack->in = stack->in_list->obj;
                stack->out = NULL;
                stack->in_list = NULL;
                stack->out_list = NULL;
                stack->ret = NULL;
                return 0;
            } else if(stack->in_list->next != NULL) {
                stack->in = stack->in_list->next->obj;
                stack->out = NULL;
                stack->in_list = NULL;
                stack->out_list = NULL;
                stack->ret = NULL;
                return 0;
            } else {
                stack->ret = nil_obj;
                stack_pop();
            }
        }
    } else if(obj_eq_sym(list->obj, "def")) {
        if(stack->ret == NULL) {
            list = list->next;
            stack->in_list = list;
            stack_push(list->next->obj, apply_exe);
        } else {
            envir_set(curr_envir, stack->in_list->obj, stack->ret);
            stack_pop();
        }
    } else {
        return 0;
    }
    return 1;
}

int is_macro_call(Object *ast) {
    if(ast->type == listt) {
        List *list = ast->data.ptr;
        Object *fun = list->obj;
        if(fun->type == symt) {
            Object *obj = envir_search(curr_envir, fun);
            if(obj != NULL && obj->type == macrot) {
                stack->in_list = list;
                stack->out = obj;
                return 1;
            }
        }
    }
    return 0;
}

void apply() {
    if(obj_is_empty_list(stack->in)) {
        // () => nil
        stack->ret = nil_obj;
        stack_pop();
    } else if(special_form()) {
        ;
    } else if(is_macro_call(stack->in)) {
        List *list = stack->in->data.ptr;
        // Object *macrofn = envir_search(list->obj, fun);
        Object *macrofn = stack->out;
        List *args = macrofn->data.func.args;
        Object *expr = macrofn->data.func.expr;
        int ret = envir_push(args, list->next);
        if(ret < 0) {
            stack->ret = err_init("wrong number of args for macro");
            stack_pop();
            return;
        }
        stack->exe_mode = macro_exe;
        stack_push(expr, eval_exe);
    } else if(stack->ret == NULL) {
        stack_push(stack->in, eval_exe);
    } else if(stack->ret->type != listt) {
        // eval returned a symbol
        stack_pop();
    } else if(stack->out != NULL) {
        // only occurs after evaluating lisp function
        envir_pop();
        stack_pop();
    } else if(stack->ret->type == listt) {
        stack->out = stack->ret;
        stack->out_list = stack->out->data.ptr;
        Object *fun = stack->out_list->obj;
        List *args = stack->out_list->next;

        if(fun->type == natfnt) {
            stack->ret = fun->data.fn_ptr(args);
            stack_pop();
        } else if(fun->type == fnt) {
            int ret = envir_push(fun->data.func.args, args);
            if(ret < 0) {
                stack->ret = err_init("wrong number of args for function");
                stack_pop();
            }
            stack_push(fun->data.func.expr, apply_exe);
        } else {
            stack->ret = err_init("object is not a function");
            stack_pop();
        }
    }
}

Object *evaluate(Envir *envir, Object *ast) {
    stack_push(NULL, result_exe);
    stack_push(ast, apply_exe);
    while(stack->exe_mode != result_exe) {
        if(stack->exe_mode == eval_exe) {
            eval();
        } else if(stack->exe_mode == apply_exe) {
            apply();
        } else if(stack->exe_mode == macro_exe) {
            printf("macro expansion:\n");
            obj_debug(stack->ret);
            printf("\n");
            envir_pop();
            stack_pop();
            /*
            stack->in = stack->ret;
            stack->out = NULL;
            stack->ret = NULL;
            if(stack->in->type != listt)
                stack->exe_mode = eval_exe;
            else
                stack->exe_mode = apply_exe;
            //*/
            // stack_pop();
        } else if(stack->exe_mode == unknown) {
            printf("error: unknown execution mode\n");
            return NULL;
        }
    }
    Object *ret = stack->ret;
    stack_pop();
    return ret;
}
