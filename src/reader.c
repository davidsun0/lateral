#include <stdio.h>
#include <stdlib.h>

#include <string.h>

#include "object.h"
#include "eval.h"
#include "garbage.h"

#include "reader.h"

#define is_white_space(c) ((c) == ' ' || (c) == '\n' || (c) == ',' \
        || (c) == '\t' || (c) == '\r')

#define is_special_char(c) ((c) == '(' || (c) == ')' || (c) == '{' \
        || (c) == '}')

int read_token(char **buf, Object **obj) {
    char *start = *buf;

    // consume white space and comments
    while(*start != '\0') {
        if(is_white_space(*start)) {
            while(is_white_space(*start)) {
                start++;
            }
        } else if(*start == ';') {
            while(*start != '\0' && *start != '\n') {
                start++;
            }
        } else {
            break;
        }
    }

    char *end = start;
    if(is_special_char(*end)) {
        end ++;
    } else if(*end == '"') {
        end ++;
        while(*end != '"') {
            end ++;
            if(*end == '\0') {
                printf("error: unmatched quotation mark\n");
                return -1;
            }
        }
        end ++;
    } else {
        while(*end != '\0' && !is_white_space(*end) && !is_special_char(*end)) {
            end ++;
        }
    }

    if(start != end) {
        int len = end - start;
        *obj = obj_init_str_len(strt, start, len);
        *buf = end;
        return 0;
    } else {
        // end of buffer
        return -1;
    }
}

Object *read_atom(Object *obj) {
    if(obj->type != strt) {
        printf("error: trying to parse %s as atom\n", type_to_str(obj->type));
        obj_debug(obj);
        return NULL;
    }

    char *str = obj_string(obj);
    if('0' <= str[0] && str[0] <= '9') {
        // try to parse as number
        int sum = 0;
        int i = 0;
        while(str[i] != '\0') {
            if(str[i] < '0' || str[i] > '9') {
                printf("error: %s is not an integer\n", str);
                return NULL;
            }
            sum *= 10;
            sum += str[i] - '0';
            i ++;
        }
        union Data dat = { .int_val = sum };
        return obj_init(intt, dat);
    } else if (str[0] == '"') {
        // remove beginning and ending quotation marks
        return obj_init_str_len(strt, str + 1, strlen(str) - 2);
    } else if (str[0] == ':') {
        return obj_init_str(keywordt, str);
    } else {
        return obj_init_str(symt, str);
    }
}

int read_list(Object **tokens, Object **tree);
int read_hash(Object **tokens, Object **tree);

int read_form(Object **tokens, Object **tree) {
    if(*tokens == nil_obj || CAR(*tokens) == nil_obj) {
        return -1;
    } else if(strcmp(obj_string(CAR(*tokens)), "(") == 0) {
        // consume left paren
        *tokens = CDR(*tokens);
        return read_list(tokens, tree);
    } else if(strcmp(obj_string(CAR(*tokens)), "{") == 0) {
        // consume left bracket
        *tokens = CDR(*tokens);
        return read_hash(tokens, tree);
    } else {
        *tree = read_atom(CAR(*tokens));
        *tokens = CDR(*tokens);
        return 0;
    }
}

int read_list(Object **tokens, Object **tree) {
    *tree = NULL;
    Object *listb = *tree;

    Object *obj = NULL;
    while(*tokens != nil_obj && strcmp(obj_string(CAR(*tokens)), ")") != 0) {
        read_form(tokens, &obj);
        listb = list_append(listb, obj);
        if(*tree == NULL) {
            *tree = listb;
        }
    }

    if(*tokens == nil_obj) {
        fprintf(stderr, "syntax error: unmatched '('\n");
        return -1;
    }

    if(*tree == NULL) {
        *tree = cell_init();
    }

    // consume right paren
    *tokens = CDR(*tokens);
    return 0;
}

int read_hash(Object **tokens, Object **tree) {
    Object *key = NULL;
    Object *val = NULL;
    *tree = obj_hashmap_init(16);
    while(*tokens != nil_obj && strcmp(obj_string(CAR(*tokens)), "}") != 0) {
        read_form(tokens, &key);
        if(*tokens == nil_obj) {
            fprintf(stderr, "synatx error: expect an even number of elements\
                    in a hash map literal\n");
            return -1;
        }
        read_form(tokens, &val);
        hashmap_set((*tree)->data.hashmap, key, val);
    }

    if(*tokens == nil_obj) {
        fprintf(stderr, "syntax error: unmatched '{'\n");
        return -1;
    }

    // consume right bracket
    *tokens = CDR(*tokens);
    return 0;
}

Object *read_string(char *str) {
    Object *tokens = NULL;
    Object *curr = tokens;
    Object *obj = NULL;
    while(read_token(&str, &obj) >= 0) {
        curr = list_append(curr, obj);
        if(tokens == NULL) {
            tokens = curr;
        }
    }

    Object *ast = NULL;
    curr = tokens;
    int ret = read_form(&curr, &ast);

    if(curr != nil_obj) {
        printf("error: unexpected tokens: ");
        while(curr != nil_obj) {
            obj_print(CAR(curr), 0);
            printf(" ");
            curr = CDR(curr);
        }
        printf("\n");
    }

    if(ret >= 0) {
        return ast;
    } else {
        printf("read_form error\n");
        obj_debug(ast);
        return NULL;
    }

    return tokens;
}

void read_file(char *path) {
    // FILE INTERPRETER
    FILE *f = fopen(path, "r");
    char *buffer = NULL;
    int length;
    if(f != NULL) {
        fseek(f, 0, SEEK_END);
        length = ftell(f);
        fseek(f, 0, SEEK_SET);
        buffer = malloc(sizeof(char) * (length + 1));
        if(buffer == NULL) {
            fprintf(stderr, "out of memory while loading file\n");
            exit(1);
        }
        fread(buffer, 1, length, f);
        buffer[length] = '\0';
    } else {
        printf("failed to open file %s\n", path);
        return;
    }

    Object *tokens = NULL; 
    Object *curr = tokens;
    Object *obj = NULL;
    char *lbuf = buffer;
    while(read_token(&lbuf, &obj) >= 0) {
        curr = list_append(curr, obj);
        if(tokens == NULL) {
            tokens = curr;
        }
    }
    fclose(f);
    free(buffer);

    // insert program into environment to avoid garbage collection
    Object *prog = obj_init_str(strt, "*PROG*");
    envir_set(curr_envir, prog, tokens);

    // list_debug0(tokens, 0);
    Object *ast = NULL;
    curr = tokens;
    while(curr != nil_obj) {
        read_form(&curr, &ast);
        evaluate(curr_envir, ast);
        garbage_run();
    }
    if(hashmap_rem(curr_envir->map, prog) != 1) {
        printf("something has gone horribly wrong\n");
    }
    garbage_run();
}

