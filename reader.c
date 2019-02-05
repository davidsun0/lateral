#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "object.h"
#include "list.h"

#include "reader.h"

int is_white_space(char c){
    if(c == ' ' || c == ',' || c == '\t' || c == '\n' || c == '\r') {
        return 1;
    } else {
        return 0;
    }
}

int is_special_char(char c){
    if(c == '(' || c == ')' ||
            c == '[' || c == ']' || 
            c == '{' || c == '}' || 
            c == '\'' || c == '`' || c == '~' ||
            c == '^' || c == '@') {
        return 1;
    } else {
        return 0;
    }
}

struct List* list_append_str(struct List* list, char* str, int len) {
    enum object_type type;
    union Data data;

    if(len == 1) {
        type = character;
        data.character = str[0];
    } else {
        type = string;
        char* sym = malloc(sizeof(char) * (len + 1));
        strncpy(sym, str, len);
        sym[len] = '\0';
        data.ptr = sym;
    }
    return list_append(list, type, data);
}

struct List* read_tokenize(char* str) {
    struct List* list = malloc(sizeof(struct List));
    list->obj.type = empty;
    list->next = NULL;
    struct List* tail = list;

    int i = 0;
    while(str[i] != '\0') {
        if(is_white_space(str[i])) {
            // ignore whitespace
            i ++;
        } else if(str[i] == '~' && str[i + 1] == '@'){
            // capture ~@
            tail = list_append_str(tail, str + i, 2);
            i += 2;
        } else if(is_special_char(str[i])) {
            // capture single special character
            tail = list_append_str(tail, str + i, 1);
            i ++;
        } else if(str[i] == '"') {
            // capture quoted text, ignoring escaped quotes
            int j = 1;
            while(!(str[i + j] == '"' && str[i + j - 1] != '\\') &&
                    str[i + j] != '\0') {
                j ++;
            }

            // check for unclosed quotes
            if(str[i + j] == '\0') {
                // TODO: abort tokenizing
                printf("error: unclosed quote in %.*s\n", j, str + i);
            } else {
                // capture ending quote
                j ++;
                tail = list_append_str(tail, str + i, j);
            }
            i += j;
        } else if(str[i] == ';') { 
            // captures all text following ; until a new line
            int j = 1;
            while(str[i + j] != '\n' && str[i + j] != '\0') {
                j ++;
            }
            tail = list_append_str(tail, str + i, j);
            i += j;
        } else {
            // capture regular symbols
            int j = 1;
            while(!is_white_space(str[i + j]) && !is_special_char(str[i + j])
                    && str[i + j] != '\0') {
                j ++;
            }
            tail = list_append_str(tail, str + i, j);
            i += j;
        }
    }

    // list_print(list);
    free(str);
    return list;
}

struct Object* read_atom(struct List** tokens) {
    // printf("read_atom %p\n", (void*) *tokens);
    struct Object* obj = malloc(sizeof(struct Object));

    char* dat;
    char chararr[2];

    if((*tokens)->obj.type == character) {
        dat = chararr;
        chararr[0] = (*tokens)->obj.data.character;
        chararr[1] = '\0';
    } else {
        dat = (*tokens)->obj.data.ptr;
    }

    if(dat[0] == '"'){
        // parse quoted strings
        int length = strlen(dat);
        char* str = malloc(sizeof(char) * (length + 1));
        int j = 0;
        for(int i = 0; i <= length; i ++) {
            if(dat[i] == '\\') {
                switch(dat[++i]) {
                    case '"':
                        str[j] = '"';
                        break;
                    case '\\':
                        str[j] = '\\';
                        break;
                    case 'n':
                        str[j] = '\n';
                        break;
                    default:
                        //TODO: abort syntax tree building
                        printf("error: unrecognized escape sequence\n");
                        break;
                }
            } else {
                str[j] = dat[i];
            }
            j ++;
        }
        obj->type = string;
        obj->data.ptr = str;
    } else if ('0' <= dat[0] && dat[0] <= '9') {
        // parse integer
        // TODO: negative number parsing, float parsing
        int value = 0;
        while(*dat != '\0') {
            if(*dat < '0' || '9' < *dat) {
                //TODO: abort syntax tree building
                printf("error: failed to parse integer\n");
                break;
            }
            value *= 10;
            value += *dat - '0';
            dat ++;
        }
        obj->type = integer;
        obj->data.integer = value;
    } else {
        // parse symbols
        int length = strlen(dat);
        char* str = malloc(sizeof(char) * (length + 1));
        strcpy(str, dat);
        obj->type = symbol;
        obj->data.ptr = str;
    }

    // move to next token in list
    *tokens = (*tokens)->next;
    return obj;
}

struct Object* read_list(struct List**);

struct Object* read_form(struct List** tokens) {
    // printf("read_form %p\n", (void*) *tokens);
    if(object_equals_char(&((*tokens)->obj), '(')) {
        *tokens = (*tokens)->next;
        return read_list(tokens);
    } else {
        return read_atom(tokens);
    }
}

struct Object* read_list(struct List** tokens) {
    // printf("read_list %p\n", (void*) *tokens);
    struct List* list = malloc(sizeof(struct List));
    list->obj.type = empty;
    list->next = NULL;
    struct List* tail = list;
    struct Object* obj;
    while(*tokens != NULL && !object_equals_char(&((*tokens)->obj), ')')) {
        obj = read_form(tokens);
        tail = list_append_object(tail, obj);
        free(obj);
    }
    if(*tokens == NULL) {
        // TODO: abort syntax tree building
        printf("error: mismatched parens\n");
        return NULL;
    }
    tail->next = NULL;
    *tokens = (*tokens)->next;

    obj = malloc(sizeof(struct Object));
    obj->type = list_type;
    obj->data.ptr = list;
    return obj;
}

struct Object* read_string(char* str) {
    if(str == NULL || str[0] == '\0') {
        return NULL;
    }
    struct List* tokens = read_tokenize(str);
    struct List* tokens_head = tokens;
    struct Object* obj = read_form(&tokens);
    if(tokens != NULL) {
        printf("error: unexpected token\n");
        object_print_debug(&tokens->obj);
        // TODO: abort reading
    }
    // printf("tokens: %p\t%p\n", (void *) tokens, (void *) tokens_head);
    list_free(tokens_head);
    // object_print_debug(obj);
    return obj;
}

