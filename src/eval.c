#include <stdlib.h>
#include <stdio.h>

#include "object.h"

#include "eval.h"

Envir *envir_push(Envir *envir, Object *args, Object *vals) {
    int sizea = list_length(args);
    int sizeb = list_length(vals);

    if(sizea != sizeb || sizea < 0 || sizeb < 0) {
        obj_print(args, 0);
        printf("\n");
        obj_print(vals, 0);
        printf("\n");
        printf("expected %d arguments, but got %d\n", sizea, sizeb);
        return NULL;
    }

    Envir *inner = envir_init(sizea * 2);
    for(int i = 0; i < sizea; i ++) {
        envir_set(inner, CAR(args), CAR(vals));
        args = CDR(args);
        vals = CDR(vals);
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
        // Object *fn = ast->data.cell.car;
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
        Object *vals = CDR(ast);

        Object *args = macro->data.func.args;
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
            obj_debug(ast);
            ret = err_init("symbol not found in envir");
        }
        return ret;
    } else if(ast->type == listt) {
        // Object *list = cell_init();
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
    if(CAR(ast) == nil_obj && CDR(ast) == nil_obj) {
        return ast;
    }

    ast = macro_expand(envir, ast);

    if(ast->type == listt) {
        if(obj_eq_sym(CAR(ast), "def")) {
            Object *sym = CAR(CDR(ast));
            Object *val = evaluate(envir, CAR(CDR(CDR(ast))));
            envir_set(envir, sym, val);
            return val;
        } else if(obj_eq_sym(CAR(ast), "defmacro")) {
            Object *name = CAR(CDR(ast));
            Object *args = CAR(CDR(CDR(ast)));
            Object *expr = CAR(CDR(CDR(CDR(ast))));
            if(args->type != listt) {
                return err_init("macro args must be a list");
            }

            union Data dat = { .func = { .args = args, .expr = expr }};
            Object *macro = obj_init(macrot, dat);
            envir_set(envir, name, macro);
            return name;
        } else if(obj_eq_sym(CAR(ast), "lambda")) {
            Object *args = CAR(CDR(ast));
            Object *expr = CAR(CDR(CDR(ast)));
            union Data dat = { .func = { .args = args, .expr = expr }};
            return obj_init(fnt, dat);
        } else if(obj_eq_sym(CAR(ast), "if")) {
            Object *pred = evaluate(envir, CAR(CDR(ast)));
            ast = CDR(CDR(ast));
            if(pred != nil_obj) {
                // evaluate true branch
                return evaluate(envir, CAR(ast));
            } else {
                if(CDR(ast) == nil_obj) {
                    // nil if no false branch
                    return nil_obj;
                } else {
                    // false branch
                    return evaluate(envir, CAR(CDR(ast)));
                }
            }
        } else if(obj_eq_sym(CAR(ast), "quote")) {
            return CAR(CDR(ast));
        } else if(obj_eq_sym(CAR(ast), "progn")) {
            ast = CDR(ast);
            while(CDR(ast) != nil_obj) {
                evaluate(envir, CAR(ast));
                ast = CDR(ast);
            }
            return evaluate(envir, CAR(ast));
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
            Object *args = fn->data.func.args;
            envir = envir_push(envir, args, vals);
            if(envir == NULL) {
                printf("failed to create envir\n");
                printf("funcall: \n");
                // obj_debug(funcall);
                obj_print(funcall, 0);
                printf("\nexpr: \n");
                // obj_debug(ast);
                obj_print(ast, 0);
                printf("\n");
                exit(1);
            }
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
