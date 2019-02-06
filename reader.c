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

void list_append_str(struct Object* list, char* str, int len) {
    enum object_type type;
    union Data data;

    if(len == 1) {
        type = char_type;
        data.char_type = str[0];
    } else {
        type = string;
        char* sym = malloc(sizeof(char) * (len + 1));
        strncpy(sym, str, len);
        sym[len] = '\0';
        data.ptr = sym;
    }
    list_append(list, type, data);
}

struct Object* read_tokenize(char* str) {
    struct Object* list = list_init();

    int i = 0;
    while(str[i] != '\0') {
        if(is_white_space(str[i])) {
            // ignore whitespace
            i ++;
        } else if(str[i] == '~' && str[i + 1] == '@'){
            // capture ~@
            list_append_str(list, str + i, 2);
            i += 2;
        } else if(is_special_char(str[i])) {
            // capture single special character
            list_append_str(list, str + i, 1);
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
                list_append_str(list, str + i, j);
            }
            i += j;
        } else if(str[i] == ';') {
            // ; signifies a comment. Ignore all characters until new line
            // int j = 1;
            while(str[i] != '\n' && str[i] != '\0') {
                i ++;
            }
            // list_append_str(list, str + i, j);
            // i += j;
        } else {
            // capture regular symbols
            int j = 1;
            while(!is_white_space(str[i + j]) && !is_special_char(str[i + j])
                    && str[i + j] != '\0') {
                j ++;
            }
            list_append_str(list, str + i, j);
            i += j;
        }
    }

    // object_print_debug(list);
    free(str);
    return list;
}

struct Object* read_atom(struct List** tokens) {
    // printf("read_atom %p\n", (void*) *tokens);
    struct Object* obj = malloc(sizeof(struct Object));

    char* dat;
    char chararr[2];

    if((*tokens)->obj->type == char_type) {
        dat = chararr;
        chararr[0] = (*tokens)->obj->data.char_type;
        chararr[1] = '\0';
    } else {
        dat = (*tokens)->obj->data.ptr;
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
    } else if (dat[0] == '-' || ('0' <= dat[0] && dat[0] <= '9')) {
        // parse integer
        // TODO: float parsing
        int value = 0;
        int negative = 0;
        if(dat[0] == '-') {
            negative = 1;
            dat ++;
        }
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
        if(negative) {
            value = -1 * value;
        }
        obj->type = int_type;
        obj->data.int_type = value;
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
    if(object_equals_char((*tokens)->obj, '(')) {
        *tokens = (*tokens)->next;
        return read_list(tokens);
    } else {
        return read_atom(tokens);
    }
}

struct Object* read_list(struct List** tokens) {
    // printf("read_list %p\n", (void*) *tokens);
    struct Object* list = list_init();

    struct Object* obj;
    while(*tokens != NULL && !object_equals_char((*tokens)->obj, ')')) {
        obj = read_form(tokens);
        list_append_object(list, obj);
    }
    if(*tokens == NULL) {
        // TODO: abort syntax tree building
        printf("error: mismatched parens\n");
        return NULL;
    }
    *tokens = (*tokens)->next;

    return list;
}

struct Object* read_string(char* str) {
    if(str == NULL || str[0] == '\0') {
        return NULL;
    }
    struct Object* tokens = read_tokenize(str);
    struct List* token_list = (struct List*) tokens->data.ptr;
    struct Object* obj = read_form(&token_list);

    if(token_list != NULL) {
        printf("error: unexpected token\n");
        // object_print_debug(tokens->obj);
    }

    // object_print_debug(obj);
    return obj;
}

