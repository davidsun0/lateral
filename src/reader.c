#include <stdio.h>
#include <stdlib.h>

#include <string.h>

#include "object.h"

#include "reader.h"

#define is_white_space(c) ((c) == ' ' || (c) == '\n' || (c) == ',' || \
        (c) == '\t' || (c) == '\r')

#define is_special_char(c) ((c) == '(' || (c) == ')' )

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
        char *str = malloc(sizeof(char) * (len + 1));
        strncpy(str, start, len);
        str[len] = '\0';
        
        union Data d = { .ptr = str };
        *obj = obj_init(strt, d);
        *buf = end;
        return 0;
    } else {
        // end of buffer
        return -1;
    }
}

char *la_strdup(char *str) {
    int len = 0;
    while(str[len] != '\0') {
        len ++;
    }

    char *new = malloc(sizeof(char) * (len + 1));
    for(int i = 0; i < len; i ++) {
        new[i] = str[i];
    }
    new[len] = '\0';
    return new;
}

Object *read_atom(Object *obj) {
    if(obj->type != strt) {
        printf("error: trying to parse non-string to atom\n");
        obj_debug(obj);
        return NULL;
    }

    char *str = (char *)obj->data.ptr;
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
        char *nstr = la_strdup(str + 1);
        int i = 0;
        while(nstr[i] != '\0') {
            i ++;
        }
        nstr[i - 1] = '\0';
        union Data dat = { .ptr = nstr };
        return obj_init(strt, dat);
    } else if (str[0] == ':') {
        char *nstr = la_strdup(str);
        union Data dat = { .ptr = nstr };
        return obj_init(keywordt, dat);
    } else {
        char *nstr = la_strdup(str);
        union Data dat = { .ptr = nstr };
        return obj_init(symt, dat);
    }
}

int read_list(Object **tokens, Object **tree);

int read_form(Object **tokens, Object **tree) {
    if(*tokens == nil_obj || CAR(*tokens) == nil_obj) {
        printf("is this supposed to happend?");
        return -1;
    } else if(*(char *)(CAR(*tokens)->data.ptr) == '(') {
        // consume left paren
        *tokens = CDR(*tokens);
        return read_list(tokens, tree);
    } else {
        *tree = read_atom(CAR(*tokens));
        *tokens = CDR(*tokens);
        return 0;
    }
}

int read_list(Object **tokens, Object **tree) {
    *tree = cell_init();
    Object *listb = *tree;

    Object *obj = NULL;
    while(*tokens != nil_obj && *(char *)(CAR(*tokens)->data.ptr) != ')') {
        read_form(tokens, &obj);
        listb = list_append(listb, obj);
    }
    
    if(*tokens == nil_obj) {
        fprintf(stderr, "syntax error: unmatched '('\n");
        return -1;
    }

    // consume right paren
    *tokens = CDR(*tokens);
    return 0;
}

Object *read_string(char *str) {
    Object *tokens = cell_init();
    Object *curr = tokens;
    Object *obj = NULL;
    while(read_token(&str, &obj) >= 0) {
        curr = list_append(curr, obj);
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
        obj_debug(tokens);
        return NULL;
    }

    return tokens;
}
