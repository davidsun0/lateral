#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "object.h"
#include "list.h"

#include "reader.h"

#define is_white_space(c) ((c) == ' ' || (c) == '\n' || (c) == ',' || \
        (c) == '\t' || (c) == '\r')

#define is_special_char(c) ((c) == '(' || (c) == ')' || (c) == '[' || \
        (c) == ']' || (c) == '{' || (c) == '}' || (c) == '\'' || c == '`' || \
        (c) == '^' || (c) == '~')

struct Object* read_make_token(char* str, int len) {
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
    return object_init(type, data);
}

struct Object* read_emit_token(char* str, int* pos) {
    int i = 0;
    if(str[i] == '\0') {
        *pos += i;
        return NULL;
    } else if(is_white_space(str[i])) {
        while(str[i] != '\0' && is_white_space(str[i])) {
            i ++;
        }
        *pos += i;
        return NULL;
    } else if(is_special_char(str[i])) {
        i = 1;
        if(str[i - 1] == '~' && str[i] == '@') {
            i ++;
        }
        *pos += i;
        return read_make_token(str, i);
    } else if(str[i] == ';') {
        // comments
        while(str[i] != '\0' && str[i] != '\n') {
            i ++;
        }
        *pos += i;
        return NULL;
    } else if(str[i] == '"') {
        // strings
        i ++;
        while(str[i] != '\0' && str[i] != '"') {
            if(str[i] == '\\' && str[i + 1] == '"')
                i ++;
            i ++;
        }
        if(str[i] == '"') {
            // read the closing quotation
            i ++;
        } else {
            // TODO: error handling
            printf("error: unclosed quotation\n");
            printf("%c\n", str[i]);
            *pos += i;
            return NULL;
        }
        *pos += i;
        return read_make_token(str, i);
    } else {
        while(str[i] != '\0' && !is_white_space(str[i])
                && !is_special_char(str[i])) {
            i ++;
        }
        *pos += i;
        return read_make_token(str, i);
    }
}

struct Object* read_next_token(char* str, int* pos) {
    struct Object* result;
    int lastpos = *pos;
    while(1) {
        result = read_emit_token(str + *pos, pos);
        if(result != NULL) {
            break;
        } else if(lastpos == *pos) {
            return NULL;
        }
        lastpos = *pos;
    }
    return result;
}

struct Object* read_make_atom(struct Object* obj) {
    char* dat;
    char chararr[2];

    if(obj->type == char_type) {
        dat = chararr;
        chararr[0] = obj->data.char_type;
        chararr[1] = '\0';
    } else {
        dat = obj->data.ptr;
    }

    enum object_type type;
    union Data data;
    if(dat[0] == '"'){
        // parse quoted strings
        int length = strlen(dat);
        char* str = malloc(sizeof(char) * (length - 1));
        int j = 0;
        for(int i = 1; i < length - 1; i ++) {
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
        str[j] = '\0';
        type = string;
        data.ptr = str;
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
        type = int_type;
        data.int_type = value;
    } else {
        // parse symbols
        int length = strlen(dat);
        char* str = malloc(sizeof(char) * (length + 1));
        strcpy(str, dat);
        type = symbol;
        data.ptr = str;
    }
    return object_init(type, data);
}

int read_list(char*, int*, struct Object*);

int read_form(char* str, int* pos, struct Object* tree) {
    struct Object* token = read_next_token(str, pos);
    if(token == NULL) {
        return 1;
    } else if(object_equals_char(token, '(')) {
        int error = read_list(str, pos, tree);
        return error;
    } else if(object_equals_char(token, ')')) {
        return ')';
    } else if(object_equals_char(token, '\'') ||
            object_equals_char(token, '~') ||
            object_equals_char(token, '`') ||
            object_equals_string(token, "~@")) {

        struct Object* list = list_init();
        char* sym_str = malloc(sizeof(char) * 24);
        if(object_equals_char(token, '\'')) {
            strcpy(sym_str, "quote");
        } else if(object_equals_char(token, '~')) {
            strcpy(sym_str, "unquote");
        } else if(object_equals_char(token, '`')) {
            strcpy(sym_str, "quasiquote");
        } else {
            strcpy(sym_str, "unquote-splicing");
        }
        union Data data;
        data.ptr = sym_str;
        list_append(list, symbol, data);

        read_form(str, pos, list);
        list_append_object(tree, list);
        return 0;
    } else {
        struct Object* atom = read_make_atom(token);
        list_append_object(tree, atom);
        return 0;
    }
}

int read_list(char* str, int* pos, struct Object* tree) {
    struct Object* list = list_init();
    while(1) {
        int error = read_form(str, pos, list);
        if(error == ')') {
            list_append_object(tree, list);
            return 0;
        } else if(error != 0) {
            return error;
        }
    }
    return 0;
}

struct Object* read_string(char* str) {
    if(str == NULL || str[0] == '\0') {
        return NULL;
    }

    int pos = 0;
    struct Object* tree = list_init();
    read_form(str, &pos, tree);
    struct List* tree_list = (struct List*) tree->data.ptr;

    int posb = pos;
    if(read_next_token(str, &posb) != NULL) {
        printf("error: unexpected token(s)\n");
        printf("tokens: %s\n", str + pos);
    }

    return tree_list->obj;
}
