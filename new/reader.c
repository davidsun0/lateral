#include <stdio.h>
#include <stdlib.h>

#include <string.h>

#include "object.h"
#include "list.h"

#include "reader.h"

#define is_white_space(c) ((c) == ' ' || (c) == '\n' || (c) == ',' || \
        (c) == '\t' || (c) == '\r')

#define is_special_char(c) ((c) == '(' || (c) == ')' )

int read_token(char **buf, Object **obj) {
    char *start = *buf;

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
    } else {
        char *nstr = la_strdup(str);
        union Data dat = { .ptr = nstr };
        return obj_init(symt, dat);
    }
}

int read_list(List **tokens, Object **tree);

int read_form(List **tokens, Object **tree) {
    if(*tokens == NULL || (*tokens)->obj == NULL) {
        return -1;
    } else if(*(char *)(*tokens)->obj->data.ptr == '(') {
        // consume left paren
        *tokens = (*tokens)->next;

        return read_list(tokens, tree);
    } else {
        /*
        char *str = la_strdup((char *)(*tokens)->obj->data.ptr);
        union Data dat = { .ptr = str };
        Object *obj = obj_init(symt, dat);
        */
        *tree = read_atom((*tokens)->obj);
        *tokens = (*tokens)->next;
        return 0;
    }
}

int read_list(List **tokens, Object **tree) {
    List *list = list_init();
    union Data dat = { .ptr = list };
    *tree = obj_init(listt, dat);

    Object *obj = NULL;
    while(*tokens != NULL && *(char *)(*tokens)->obj->data.ptr != ')') {
        read_form(tokens, &obj);
        list = list_append(list, obj);
    }
    
    if(*tokens == NULL) {
        fprintf(stderr, "syntax error: unmatched '('\n");
        return -1;
    }

    // consume right paren
    *tokens = (*tokens)->next;
    return 0;
}

Object* read_string(char *str) {
    // tokenize input string
    List *tokens = list_init();
    Object *obj = NULL;
    List *curr = tokens;
    while(read_token(&str, &obj) >= 0) {
        curr = list_append(curr, obj);
    }

    // convert token into abstract syntax tree
    Object *ast = NULL;
   
    curr = tokens;
    int ret = read_form(&curr, &ast);

    while(tokens != NULL) {
        obj_free(tokens->obj);
        List *prev = tokens;
        tokens = tokens->next;
        free(prev);
    }
        
    if(ret >= 0) {
        // obj_debug(ast);
        return ast;
    } else {
        return NULL;
    }
}
