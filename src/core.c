#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "eval.h"
#include "reader.h"
#include "object.h"

#include "core.h"

Object *la_eval(Object *list) {
    return evaluate(curr_envir, CAR(list));
}

Object *la_apply(Object *list) {
    Object *fn = CAR(list);
    Object *arglist = CAR(CDR(list));
    if(fn->type == natfnt) {
        return fn->data.fn_ptr(arglist);
    } else if(fn->type == fnt) {
        Envir* envir = envir_push(curr_envir, fn->data.func.params, arglist);
        if(envir == NULL) {
            return err_init("failed to create envir");
        }
        Object *ret = evaluate(envir, fn->data.func.expr);
        envir_pop(envir);
        return ret;
    } else {
        return err_init("object is not a function");
    }
}

Object *la_read_file(Object* list) {
    Object *str = CAR(list);
    char *file_name = obj_string(str);
    read_file(file_name);
    return nil_obj;
}

Object *la_sum(Object *list) {
    int sum = 0;
    while(list != nil_obj) {
        Object *obj = CAR(list);
        if(obj->type == intt) {
            sum += obj->data.int_val;
        } else {
            return err_init("type error");
        }
        list = CDR(list);
    }
    union Data dat = { .int_val = sum };
    return obj_init(intt, dat);
}

Object *la_diff(Object *list) {
    int diff;
    Object *obj = CAR(list);
    if(obj->type == intt) {
        diff = obj->data.int_val;
    } else {
        return err_init("type error");
    }

    if(CAR(CDR(list)) == nil_obj) {
        union Data dat = { .int_val = -1 * diff };
        return obj_init(intt, dat);
    } else {
        list = CDR(list);
        while(list != nil_obj) {
            Object *obj = CAR(list);
            if(obj->type == intt) {
                diff -= obj->data.int_val;
            } else {
                return err_init("type error");
            }
            list = CDR(list);
        }
        union Data dat = { .int_val = diff };
        return obj_init(intt, dat);
    }
}

Object *la_mult(Object *list) {
    int product = 1;
    while(list != nil_obj) {
        Object *obj = CAR(list);
        if(obj->type == intt) {
            product *= obj->data.int_val;
        } else {
            return err_init("type error");
        }
        list = CDR(list);
    }
    union Data dat = { .int_val = product };
    return obj_init(intt, dat);
}

Object *la_divide(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));
    if(a->type != intt || b->type != intt) {
        return err_init("type error");
    }
    int ai = a->data.int_val;
    int bi = b->data.int_val;
    union Data dat = { .int_val = ai / bi };
    return obj_init(intt, dat);
}

Object *la_bitand(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));
    if(a->type != intt || b->type != intt) {
        return err_init("type error");
    }
    int ai = a->data.int_val;
    int bi = b->data.int_val;
    union Data dat = { .int_val = ai & bi };
    return obj_init(intt, dat);
}

Object *la_bit_asr(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));
    if(a->type != intt || b->type != intt) {
        return err_init("type error");
    }
    int ai = a->data.int_val;
    int bi = b->data.int_val;
    union Data dat = { .int_val = ai >> bi };
    return obj_init(intt, dat);
}

Object *la_modulo(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));
    if(a->type != intt || b->type != intt) {
        return err_init("type error");
    }
    int ai = a->data.int_val;
    int bi = b->data.int_val;
    union Data dat = { .int_val = ai % bi };
    return obj_init(intt, dat);
}

Object *la_lt(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));
    if(a->type != intt || b->type != intt) {
        return err_init("type error");
    } else if(a->data.int_val < b->data.int_val) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

Object *la_eq(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));
    if(a->type != intt || b->type != intt) {
        return err_init("type error");
    } else if(a->data.int_val == b->data.int_val) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

Object *la_identical(Object *list) {
    if(CAR(list) == CAR(CDR(list))) {
        return tru_obj;
    } else {
        return nil_obj;
    }
}

Object *la_equal(Object *list) {
    Object *a = CAR(list);
    Object *b = CAR(CDR(list));

    if(a == b) {
        return tru_obj;
    }

    if(a->type != b->type) {
        return nil_obj;
    }

    int is_equal = 0;
    if(a->type == symt || a->type == strt || a->type == keywordt) {
        is_equal = strcmp(obj_string(a), obj_string(b)) == 0;
    } else if(a->type == intt) {
        is_equal = a->data.int_val == b->data.int_val;
    } else if(a->type == floatt) {
        is_equal = a->data.float_val == b->data.float_val;
    } else {
        fprintf(stderr, "la_equal not implemented for %s type\n",
                type_to_str(a->type));
    }

    return is_equal ? tru_obj : nil_obj;
}

Object *la_type(Object *list) {
    Object *a = CAR(list);
    if(a->type == symt) {
        return obj_init_str(keywordt, "symbol");
    } else if(a->type == strt) {
        return obj_init_str(keywordt, "string");
    } else if(a->type == keywordt) {
        return obj_init_str(keywordt, "keyword");
    } else if(a->type == chart) {
        return obj_init_str(keywordt, "char");
    } else if(a->type == intt) {
        return obj_init_str(keywordt, "int");
    } else if(a->type == floatt) {
        return obj_init_str(keywordt, "float");
    } else if(a->type == listt) {
        return obj_init_str(keywordt, "list");
    } else if(a->type == hashmapt) {
        return obj_init_str(keywordt, "hashmap");
    } else if(a->type == natfnt) {
        return obj_init_str(keywordt, "function");
    } else if(a->type == fnt) {
        return obj_init_str(keywordt, "function");
    } else if(a->type == macrot) {
        return obj_init_str(keywordt, "macro");
    } else {
        return err_init("error: corrupted object");
    }
}

Object *la_to_keyword(Object *list) {
    Object *a = CAR(list);
    if(a->type == symt || a->type == strt) {
        return obj_init_str(keywordt, obj_string(a));
    } else if(a->type == keywordt) {
        return a;
    } else {
        return err_init("cannot convert to keyword");
    }
}

Object *la_to_symbol(Object *list) {
    Object *a = CAR(list);
    if(a->type == strt || a->type == keywordt) {
        return obj_init_str(symt, obj_string(a));
    } else if(a->type == symt) {
        return a;
    } else {
        return err_init("cannot convert to symbol");
    }
}

/*
 * FUNCTION FUNCTIONS
 *
 */
Object *la_func_params(Object *list) {
    if(CAR(list)->type == fnt) {
        return CAR(list)->data.func.params;
    } else {
        return err_init("type error");
    }
}

Object *la_func_expr(Object *list) {
    if(CAR(list)->type == fnt) {
        return CAR(list)->data.func.expr;
    } else {
        return err_init("type error");
    }
}

/*
 * LIST FUNCTIONS
 *
 */
Object *la_list(Object *list) {
    Object *ret = NULL;
    Object *retb = ret;
    while(list != nil_obj) {
        retb = list_append(retb, CAR(list));
        if(ret == NULL) {
            ret = retb;
        }
        list = CDR(list);
    }
    return ret;
}

Object *la_car(Object *list) {
    if(CAR(list)->type == listt) {
        return CAR(CAR(list));
    } else {
        printf("%s type in car\n", type_to_str(CAR(list)->type));
        return err_init("type error in car\n");
    }
}

Object *la_cdr(Object *list) {
    if(CAR(list)->type == listt) {
        return CDR(CAR(list));
    } else {
        printf("%s type in cdr\n", type_to_str(CAR(list)->type));
        return err_init("type error in cdr\n");
    }
}

Object *la_cons(Object *list) {
    Object *ret = cell_init();
    CAR(ret) = CAR(list);
    CDR(ret) = CAR(CDR(list));
    return ret;
}

Object *la_reverse_mut(Object *list) {
    Object *prev = nil_obj;
    Object *next = nil_obj;
    Object *curr = CAR(list);

    while(curr != nil_obj) {
        next = CDR(curr);
        CDR(curr) = prev;
        prev = curr;
        curr = next;
    }
    return prev;
}

#define FLATTEN_DEPTH 512
// because lisp flatten is too slow :(
Object *la_flatten(Object *list) {
    Object *tree = CAR(list);
    if(tree == nil_obj || tree->type != listt)
        return tree;
    else {
        Object *tree_stack[FLATTEN_DEPTH];
        int stack_top = 0;

        Object *output = NULL;
        Object *out_tail = NULL;

        while(stack_top != 0 || tree != nil_obj) {
            if(tree == nil_obj) {
                stack_top --;
                if(stack_top < 0)
                    break;
                else
                    tree = tree_stack[stack_top];
            }
            // if the car is a list
            else if(CAR(tree) != nil_obj && CAR(tree)->type == listt) {
                // push CDR(tree) onto the stack
                tree_stack[stack_top] = CDR(tree);
                stack_top ++;
                if(stack_top >= FLATTEN_DEPTH) {
                    printf("flatten can't handle more than %d nested lists\n",
                            FLATTEN_DEPTH);
                    obj_debug(tree);
                    return NULL;
                }
                tree = CAR(tree);
            } else {
                out_tail = list_append(out_tail, CAR(tree));
                if(output == NULL)
                   output = out_tail;

                tree = CDR(tree);
                /*
                if(tree == nil_obj) {
                    // pop off of stack
                    stack_top --;
                    if(stack_top < 0)
                        break;
                    else
                        tree = tree_stack[stack_top];
                }
                */
            }
        }
        return output;
    }
}

/*
 * HASHMAP FUNCTIONS
 *
 */

Object *la_hashmap_init(Object *list) {
    Object *size = CAR(list);
    if(size->type == intt) {
        return obj_hashmap_init(size->data.int_val);
    } else {
        return err_init("map init expects int size");
    }
}

Object *la_hashmap_get(Object *list) {
    Object *hashmap = CAR(list);
    Object *key = CAR(CDR(list));
    Object *res = hashmap_get(hashmap->data.hashmap, key);

    Object *ret = cell_init();
    CDR(ret) = cell_init();
    if(res == NULL) {
        CAR(ret) = NIL;
        CAR(CDR(ret)) = NIL;
    } else {
        CAR(ret) = res;
        CAR(CDR(ret)) = tru_obj;
    }
    return ret;
}

Object *la_hashmap_set(Object *list) {
    Object *hashmap = CAR(list);
    if(hashmap->type != hashmapt) {
        return err_init("arg is not of type hashmap");
    }
    Object *key = CAR(CDR(list));
    Object *val = CAR(CDR(CDR(list)));
    hashmap_set(hashmap->data.hashmap, key, val);
    return hashmap;
}

Object *la_maphash(Object *list) {
    Object *fun = CAR(list);
    Object *hashobj = CAR(CDR(list));
    HashMap *hashmap = hashobj->data.hashmap;
    for(int i = 0; i < hashmap->capacity; i ++) {
        Object* bucket = hashmap->buckets + i;
        while(CAR(bucket) != nil_obj) {
            Object *key = CAR(CAR(bucket));
            Object *val = CDR(CAR(bucket));

            Object *valcell = cell_init();
            CAR(valcell) = val;
            Object *keycell = cell_init();
            CAR(keycell) = key;
            CDR(keycell) = valcell;

            /*
            Object *funcall = cell_init();
            CAR(funcall) = fun;
            CDR(funcall) = keycell;
            */
            // evaluate(curr_envir, funcall);
            funcall2(fun, keycell);

            bucket = CDR(bucket);
        }
    }
    return nil_obj;
}

/*
 * STRING FUNCTIONS
 *
 */

Object *la_to_string(Object *list) {
    Object *obj = CAR(list);
    if(obj->type == symt  || obj->type == keywordt) {
        return obj_init_str(strt, obj_string(obj));
    } else if(obj->type == strt) {
        return obj;
    } else {
        printf("error: can't make %s into string\n", type_to_str(obj->type));
        return nil_obj;
    }
}

Object *la_char_at(Object *list) {
    Object *idx = CAR(list);
    Object *str = CAR(CDR(list));
    if(str->type != strt) {
        printf("error: can't take char at from %s type\n",
            type_to_str(str->type));
        return err_init("type error");
    } else if(idx->type != intt) {
        printf("error: char at index must be int, not %s type\n",
            type_to_str(idx->type));
        return err_init("type error");
    } else {
        if(idx->data.int_val < 0) {
            return nil_obj;
        }
        char *s = obj_string(str);
        for(int i = 0; i <= idx->data.int_val; i ++) {
            if(s[i] == '\0') {
                return nil_obj;
            }
        }
        return obj_init_str_len(chart, s + idx->data.int_val, 1);
    }
}

Object *la_char_int(Object *list) {
    Object *chr = CAR(list);
    if(chr->type != chart) {
        return err_init("type error");
    }
    union Data dat = { .int_val = obj_string(chr)[0] };
    return obj_init(intt, dat);
}

Object *la_str_cat(Object *list) {
    int len = 0;
    int capacity = SHORT_STRING_LENGTH;
    char *buff = malloc(capacity);
    if(!buff) {
        fprintf(stderr, "out of memory while allocating string in la_str_cat\n");
        exit(1);
    }
    while(list != nil_obj) {
        Object *s = CAR(list);
        if(s->type != strt && s->type != chart) {
            free(buff);
            printf("str cat expects string type, not %s\n", type_to_str(s->type));
            return err_init("type error");
        }
        char *str = obj_string(s);
        while(*str != '\0') {
            buff[len++] = *str;
            str ++;
            if(len == capacity - 2) { // zero index + null terminator
                capacity *= 2;
                buff = realloc(buff, capacity);
                if(!buff) {
                    fprintf(stderr, "out of memory while resizing string in la_str_cat\n");
                    exit(1);
                }
            }
        }
        list = CDR(list);
    }
    buff[len] = '\0';
    Object *ret = obj_init_str_len(strt, buff, len);
    free(buff);
    return ret;
}

Object *la_print(Object *list) {
    obj_print(CAR(list), 0);
    printf("\n");
    return nil_obj;
}

Object *la_pprint(Object *list) {
    obj_print(CAR(list), 1);
    printf("\n");
    return nil_obj;
}

Object *la_debug(Object *list) {
    obj_debug(CAR(list));
    return nil_obj;
}

Object *la_write_bytes(Object *list) {
    Object* file_name = CAR(list);
    Object* byte_list = CAR(CDR(list));
    if(file_name->type != strt) {
        return err_init("type error");
    }
    FILE *f = fopen(obj_string(file_name), "wb");
    if(f != NULL) {
        while(byte_list != nil_obj) {
            if(CAR(byte_list)->type != intt) {
                return err_init("type error");
            }
            char lowest = CAR(byte_list)->data.int_val & 0xFF;
            fwrite(&lowest, sizeof(char), 1, f);
            byte_list = CDR(byte_list);
        }
        return tru_obj;
    } else {
        return err_init("failed to open file");
    }
}

void insert_function(char *name, Object *(fn_ptr)(Object *)) {
    union Data dat = { .fn_ptr = fn_ptr };
    Object *fn = obj_init(natfnt, dat);

    Object *sym = obj_init_str(symt, name);
    envir_set(curr_envir, sym, fn);
}

void lang_init() {
    union Data dat = { .int_val = 0 };
    tru_obj = obj_init(intt, dat);
    envir_set_str(curr_envir, "t", tru_obj);
    envir_set_str(curr_envir, "nil", nil_obj);

    insert_function("include", la_read_file);
    insert_function("eval", la_eval);
    insert_function("apply", la_apply);

    insert_function("+", la_sum);
    insert_function("-", la_diff);
    insert_function("*", la_mult);
    insert_function("%", la_modulo);
    insert_function("//", la_divide);
    insert_function("bit-and", la_bitand);
    insert_function("bit-asr", la_bit_asr);
    insert_function("<", la_lt);
    insert_function("=", la_eq);
    insert_function("eq?", la_identical);
    insert_function("equal?0", la_equal);
    insert_function("type", la_type);

    insert_function("keyword", la_to_keyword);
    insert_function("symbol", la_to_symbol);

    // function functions
    insert_function("params", la_func_params);
    insert_function("expr", la_func_expr);

    // list functions
    insert_function("list", la_list);
    insert_function("car", la_car);
    insert_function("cdr", la_cdr);
    insert_function("cons", la_cons);
    insert_function("reverse!", la_reverse_mut);
    insert_function("flatten", la_flatten);

    // map functions
    insert_function("make-hashmap", la_hashmap_init);
    insert_function("hashmap-get", la_hashmap_get);
    insert_function("hashmap-set!", la_hashmap_set);
    insert_function("maphash", la_maphash);

    // string functions
    insert_function("string0", la_to_string);
    insert_function("char-at", la_char_at);
    insert_function("char-int", la_char_int);
    insert_function("str-cat", la_str_cat);

    insert_function("write-bytes", la_write_bytes);

    insert_function("print", la_print);
    insert_function("pprint", la_pprint);
    insert_function("debug", la_debug);
}
