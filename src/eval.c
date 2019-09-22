#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "object.h"

#include "eval.h"

Object *funcall(Object *(fn_ptr)(Object *), int count, ...) {
    Object *list = NULL;
    Object *listb = list;

    va_list vl;
    va_start(vl, count);
    for(int i = 0; i < count; i ++) {
        Object* next = va_arg(vl, Object*);
        listb = list_append(listb, next);
    }
    va_end(vl);

    return fn_ptr(listb);
}

Object *funcall2(Object *fn, Object *args) {
    if(fn->type == natfnt) {
        return fn->data.fn_ptr(args);
    } else if(fn->type == fnt) {
        Object *params = fn->data.func.params;
        Envir *envir = envir_push2(params, args);
        if(envir == NULL) {
            printf("failed to create envir\n");
            printf("funcall: \n");
            obj_print(fn, 0);
            printf("\n");
            return err_init("failed to create envir");
        }
        Object *ret = evaluate(envir, fn->data.func.expr);
        envir_pop2();
        return ret;
    } else {
        return err_init("fn is not a function");
    }
}

Envir *envir_push(Envir *envir, Object *params, Object *args) {
    int sizea = list_length(params);
    int sizeb = list_length(args);

    if(sizea != sizeb || sizea < 0 || sizeb < 0) {
        obj_print(params, 0);
        printf("\n");
        obj_print(args, 0);
        printf("\n");
        printf("expected %d arguments, but got %d\n", sizea, sizeb);
        return NULL;
    }

    Envir *next = envir_init(sizea * 2);
    for(int i = 0; i < sizea; i ++) {
        envir_set(next, CAR(params), CAR(args));
        params = CDR(params);
        args = CDR(args);
    }
    envir->next = next;
    next->prev = envir;
    return next;
}

Envir *envir_push2(Object *params, Object *args) {
    int sizea = list_length(params);
    int sizeb = list_length(args);

    if(sizea != sizeb || sizea < 0 || sizeb < 0) {
        obj_print(params, 0);
        printf("\n");
        obj_print(args, 0);
        printf("\n");
        printf("expected %d arguments, but got %d\n", sizea, sizeb);
        return NULL;
    }

    Envir *next = envir_init(sizea * 2);
    for(int i = 0; i < sizea; i ++) {
        envir_set(next, CAR(params), CAR(args));
        params = CDR(params);
        args = CDR(args);
    }

    curr_envir->next = next;
    next->prev = curr_envir;
    curr_envir = next;
    return next;
}

Envir *envir_pop(Envir *envir) {
    Envir *prev = envir->prev;
    prev->next = NULL;
    envir_free(envir);
    return prev;
}

void envir_pop2() {
    Envir *prev = curr_envir->prev;
    prev->next = NULL;
    envir_free(curr_envir);
    curr_envir = prev;
}

int is_macro(Envir *envir, Object *ast) {
    if(ast->type == listt) {
        Object *fn = CAR(ast);
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
        Object *macro = envir_search(envir, CAR(ast));
        Object *args = CDR(ast);

        Object *params = macro->data.func.params;
        Envir *inner = envir_push2(params, args);
        Object *expr = macro->data.func.expr;
        ast = evaluate(inner, expr);
        // curr_envir = envir_pop(inner);
        envir_pop2();
    }
    return ast;
}

Object *eval_ast(Envir *envir, Object *ast) {
    if(ast->type == symt) {
        Object *ret = envir_search(envir, ast);
        if(ret == NULL) {
            obj_debug(ast);
            ret = err_init("symbol not found in envir");
        }
        return ret;
    } else if(ast->type == listt) {
        Object *list = NULL;
        Object *listb = list;
        while(ast != nil_obj) {
            Object *obj = evaluate(envir, CAR(ast));
            if(obj->type == errt) {
                obj_print(obj, 0);
                exit(1);
            }
            listb = list_append(listb, obj);
            if(list == NULL) {
                list = listb;
            }
            ast = CDR(ast);
        }
        return list;
    } else if(ast->type == hashmapt) {
        HashMap *map = ast->data.hashmap;
        for(int i = 0; i < map->capacity; i ++) {
            Object *keyval = map->buckets + i;
            if(CAR(keyval) != NIL) {
                Object *val = CDR(CAR(keyval));
                CDR(CAR(keyval)) = evaluate(envir, val);
            }
        }
        return ast;
    } else {
        return ast;
    }
}

Object *evaluate(Envir *envir, Object *ast) {
    if(ast->type == listt && CAR(ast) == nil_obj && CDR(ast) == nil_obj) {
        return nil_obj;
    }

    ast = macro_expand(envir, ast);

    if(ast->type == listt) {
        if(obj_eq_sym(CAR(ast), "def")) {
            Object *sym = CAR(CDR(ast));
            Object *val = evaluate(envir, CAR(CDR(CDR(ast))));
            envir_set(user_envir, sym, val);
            return val;
        } else if(obj_eq_sym(CAR(ast), "defmacro")) {
            Object *name = CAR(CDR(ast));
            Object *params = CAR(CDR(CDR(ast)));
            Object *expr = CAR(CDR(CDR(CDR(ast))));
            if(params->type != listt) {
                return err_init("macro args must be a list");
            }

            union Data dat = { .func = { .params = params, .expr = expr }};
            Object *macro = obj_init(macrot, dat);
            envir_set(envir, name, macro);
            return name;
        } else if(obj_eq_sym(CAR(ast), "lambda")) {
            Object *params = CAR(CDR(ast));
            if(CAR(params) == nil_obj) {
                params = nil_obj;
            }
            Object *expr = CAR(CDR(CDR(ast)));
            union Data dat = { .func = { .params = params, .expr = expr }};
            return obj_init(fnt, dat);
        } else if(obj_eq_sym(CAR(ast), "if")) {
            Object *pred = evaluate(envir, CAR(CDR(ast)));
            ast = CDR(CDR(ast));
            if(pred->type == listt && CAR(pred) == nil_obj
                    && CDR(pred) == nil_obj) {
                if(CDR(ast) == nil_obj) {
                    // nil if no false branch
                    return nil_obj;
                } else {
                    // false branch
                    return evaluate(envir, CAR(CDR(ast)));
                }
            } else {
                return evaluate(envir, CAR(ast));
            }
        } else if(obj_eq_sym(CAR(ast), "cond")) {
            ast = CDR(ast);
            while(ast != nil_obj) {
                if(CAR(CDR(ast)) == nil_obj) {
                    return err_init("cond expects an even number of elements");
                }
                Object *pred = evaluate(envir, CAR(ast));
                if(pred != nil_obj) {
                    return evaluate(envir, CAR(CDR(ast)));
                }
                ast = CDR(CDR(ast));
            }
            return nil_obj;
        } else if(obj_eq_sym(CAR(ast), "and")) {
            ast = CDR(ast);
            while(ast != nil_obj && evaluate(envir, CAR(ast)) != nil_obj) {
                ast = CDR(ast);
            }

            if(ast == nil_obj) {
                return tru_obj;
            } else {
                return nil_obj;
            }
        } else if(obj_eq_sym(CAR(ast), "or")) {
            ast = CDR(ast);
            while(ast != nil_obj && evaluate(envir, CAR(ast)) == nil_obj) {
                ast = CDR(ast);
            }
            return CAR(ast);
        } else if(obj_eq_sym(CAR(ast), "quote")) {
            return CAR(CDR(ast));
        } else if(obj_eq_sym(CAR(ast), "progn")) {
            ast = CDR(ast);
            while(CDR(ast) != nil_obj) {
                evaluate(envir, CAR(ast));
                ast = CDR(ast);
            }
            return evaluate(envir, CAR(ast));
        } else if(obj_eq_sym(CAR(ast), "let")) {
            Object *bindings = CAR(CDR(ast));
            Object *expr = CAR(CDR(CDR(ast)));
            int bind_len = list_length(bindings);
            // throw error if odd terms
            if(bind_len % 2 != 0) {
                return err_init("arg count: let expects even count of args");
            }

            Envir *let_envir = envir_init(bind_len * 2);
            let_envir->prev = curr_envir;
            curr_envir->next = let_envir;
            curr_envir = let_envir;
            while(bindings != NIL) {
                Object *sym = CAR(bindings);
                Object *val = CAR(CDR(bindings));
                val = evaluate(let_envir, val);
                envir_set(let_envir, sym, val);
                bindings = CDR(CDR(bindings));
            }
            Object *ret = evaluate(let_envir, expr);
            curr_envir = curr_envir->prev;
            // envir->next = NULL;
            envir_free(let_envir);
            return ret;
        }

        Object *funcall = eval_ast(envir, ast);
        if(funcall->type != listt) {
            return err_init("error: epected list in fun eval");
        }
        // funcall
        Object *fn = CAR(funcall);
        if(fn->type == natfnt) {
            return fn->data.fn_ptr(CDR(funcall));
        } else if(fn->type == fnt) {
            Object *vals = CDR(funcall);
            Object *params = fn->data.func.params;
            envir = envir_push2(params, vals);
            if(envir == NULL) {
                printf("failed to create envir\n");
                printf("funcall: \n");
                // obj_debug(funcall);
                obj_print(funcall, 0);
                printf("\nexpr: \n");
                // obj_debug(ast);
                obj_print(ast, 0);
                printf("\n");
                return err_init("failed to create envir");
            }
            Object *ret = evaluate(envir, fn->data.func.expr);
            // envir = envir_pop(envir);
            envir_pop2();
            return ret;
        } else {
            printf("Error: ");
            obj_print(fn, 0);
            printf(" is not a function ");
            printf("(Evaluated from `");
            obj_print(CAR(ast), 0);
            printf("`).\n");
            Object *fn_uneval = CAR(ast);
            Object *fn2 = envir_search(user_envir, fn_uneval);
            if(fn2 != NULL && (fn2->type == fnt || fn2->type == natfnt)) {
                printf("Did you mean to use `");
                obj_print(fn_uneval, 0);
                printf("`? (Found in outer environment)\n");
                printf("If so, rename variable to avoid name conflict.\n");
            }
            return err_init("error: object is not a function");
        }
    } else {
        return eval_ast(envir, ast);
    }
}
