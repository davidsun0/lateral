#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"
#include "core.h"

#include "eval.h"

Envir *envir_push(Envir *envir, List *args, List *vals) {
    int sizea = list_length(args);
    int sizeb = list_length(vals);
    if(sizea != sizeb) {
        printf("expected %d arguments, but got %d\n", sizea, sizeb);
        return NULL;
    }

    Envir *inner = envir_init(sizea * 2);
    for(int i = 0; i < sizea; i ++) {
        envir_set(inner, args->obj, vals->obj);
        args = args->next;
        vals = vals->next;
    }
    envir->inner = inner;
    inner->outer = envir;
    return inner;
}

Envir *envir_pop(Envir *envir) {
    Envir *outer = envir->outer;
    envir_free(envir);
    return outer;
}

int is_macro(Envir *envir, Object *ast) {
    if(ast->type == listt) {
        Object *fn = ((List *)ast->data.ptr)->obj;
        if(fn->type == symt) {
            fn = envir_search(envir, fn);
            if(fn != NULL && fn->type == macrot)
                return 1;
        }
    }
    return 0;
}

Object *macro_expand(Envir *envir, Object *ast) {
    while(is_macro(envir, ast)) {
        Object *macro = envir_search(envir, ((List *)ast->data.ptr)->obj);
        List *vals = ((List *)ast->data.ptr)->next;
        List *args = macro->data.func.args;
        Envir *inner = envir_push(envir, args, vals);
        Object *expr = macro->data.func.expr;
        ast = evaluate(inner, expr);
        envir_pop(inner);
    }
    return ast;
}

Object *eval_ast(Envir *envir, Object *ast) {
    if(ast->type == symt) {
        Object *ret = envir_search(envir, ast);
        if(ret == NULL) {
            ret = err_init("symbol not found in envir");
        }
        return ret;
    } else if(ast->type == listt) {
        List *list = list_init();
        List *list2 = list;
        List *vals = ast->data.ptr;
        while(vals != NULL) {
            Object *obj = evaluate(envir, vals->obj);
            list2 = list_append(list2, obj);
            vals = vals->next;
        }
        union Data dat = { .ptr = list };
        return obj_init(listt, dat);
    } else {
        return ast;
    }
}

Object *evaluate(Envir *envir, Object *ast) {
    if(ast->type == listt && ((List *)ast->data.ptr)->obj == NULL) {
        return ast;
    }

    ast = macro_expand(envir, ast);

    if(ast->type == listt) {
        List *list = ast->data.ptr;
        if(obj_eq_sym(list->obj, "def")) {
            Object *sym = list->next->obj;
            Object *val = evaluate(envir, list->next->next->obj);
            envir_set(envir, sym, val);
            return val;
        } else if(obj_eq_sym(list->obj, "defmacro")) {
            list = list->next;
            Object *name = list->obj;
            list = list->next;
            Object *args = list->obj;
            list = list->next;
            Object *expr = list->obj;

            List *arg_list = list_copy(args->data.ptr);
            union Data dat = { .func = { .args = arg_list, .expr = expr }};
            Object *macro = obj_init(macrot, dat);
            envir_set(envir, name, macro);
            return name;
        } else if(obj_eq_sym(list->obj, "fn")) {
            Object *args = list->next->obj;
            Object *expr = list->next->next->obj;
            List *arg_list = list_copy(args->data.ptr);
            union Data dat = { .func = { .args = arg_list, .expr = expr }};
            return obj_init(fnt, dat);
        } else if(obj_eq_sym(list->obj, "if")) {
            list = list->next;
            Object *pred = evaluate(envir, list->obj);
            list = list->next;
            Object *btrue = list->obj;
            Object *bfalse = list->next == NULL ? nil_obj : list->next->obj;
            if(pred != nil_obj) {
                return btrue;
            } else {
                return bfalse;
            }
        } else if(obj_eq_sym(list->obj, "quote")) {
            return list->next->obj;
        } else if(obj_eq_sym(list->obj, "progn")) {
            list = list->next;
            while(list->next != NULL) {
                evaluate(envir, list->obj);
                list = list->next;
            }
            return evaluate(envir, list->obj);
        }

        Object *funcall = eval_ast(envir, ast);
        if(funcall->type != listt) {
            return err_init("error: epected list in fun eval");
        }
        // funcall
        list = funcall->data.ptr;
        Object *fn = list->obj;
        if(fn->type == natfnt) {
            Object *ret = fn->data.fn_ptr(list->next);
            return ret;
        } else if(fn->type == fnt) {
            List *vals = list->next;
            List *args = fn->data.func.args;
            envir = envir_push(envir, args, vals);
            Object *ret = evaluate(envir, fn->data.func.expr);
            envir = envir_pop(envir);
            return ret;
        } else {
            obj_debug(fn);
            return err_init("error: object is not a function");
        }
    } else {
        return eval_ast(envir, ast);
    }
}
