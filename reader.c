#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "object.h"
#include "list.h"
#include "garbage.h"

#include "reader.h"

#define is_white_space(c) ((c) == ' ' || (c) == '\n' || (c) == ',' || \
        (c) == '\t' || (c) == '\r')

#define is_special_char(c) ((c) == '(' || (c) == ')' || (c) == '[' || \
        (c) == ']' || (c) == '{' || (c) == '}' || (c) == '\'' || c == '`' || \
        (c) == '^' || (c) == '~')

#define BUFFER_SIZE 64

static FILE* file;
static char* buffer;
static char* buffer_a;
static char* buffer_b;
static int offset;
static int length;
static int end_of_input;
static int end_of_file;

struct Object* read_make_token(int len) {
    enum object_type type;
    union Data data;

    if(len == 1) {
        type = char_type;
        if(offset != 0) {
            data.char_type = buffer[offset - 1];
        } else {
            char* old_buffer = buffer == buffer_a ? buffer_b : buffer_a;
            data.char_type = old_buffer[BUFFER_SIZE - 2];
        }
    } else {
        type = string;
        if(offset - len < 0) {
            char* sym = malloc(sizeof(char) * (len + 1));
            printf("READ FROM THE OTHER BUFFER\n");
            int runover = offset - len;
            printf("runover: %d\n", runover);
            char* old_buffer = buffer == buffer_a ? buffer_b : buffer_a;
            strncpy(sym, old_buffer + BUFFER_SIZE + runover - 1, -runover);
            strncpy(sym - runover, buffer, offset);
            sym[len + 1] = '\0';
            data.ptr = sym;
        } else {
            char* sym = malloc(sizeof(char) * (len + 1));
            strncpy(sym, buffer + offset - len, len);
            sym[len] = '\0';
            data.ptr = sym;
        }
    }
    struct Object* output = object_init(type, data);
    return output;
}

char read_curr() {
    return buffer[offset];
}

void read_inc() {
    if(offset < length - 1) {
        offset ++;
    } else if(file != NULL && !end_of_file) {
        char* next_buffer = buffer == buffer_a ? buffer_b : buffer_a;

        int read = fread(next_buffer, sizeof(char), BUFFER_SIZE - 1, file);
        if(read == 0) {
            if(feof(file)) {
                end_of_file = 1;
                return;
            } else {
                printf("error reading file\n");
                end_of_input = 1;
                return;
            }
        } else {
            next_buffer[read] = '\0';
            offset = 0;
            length = read;
            buffer = next_buffer;
        }
    } else {
        offset ++;
        end_of_input = 1;
    }
}

struct Object* read_emit_token() {
    char c = read_curr();
    int i = 0;
    if(c == '\0') {
        return NULL;
    } else if(is_special_char(c)) {
        read_inc();
        i ++;
        if(c == '~' && read_curr() == '@') {
            read_inc();
            i ++;
        }
        return read_make_token(i);
    } else if(is_white_space(c)) {
        while(c != '\0' && is_white_space(c)) {
            read_inc();
            c = read_curr();
        }
        return NULL;
    } else if(c == ';') {
        while(c != '\0' && c != '\n') {
            read_inc();
            c = read_curr();
        }
    } else if(c == '"') {
        read_inc();
        c = read_curr();
        i ++;
        while(c != '\0' && c != '"') {
            read_inc();
            c = read_curr();
            i ++;
        }
        if(c == '"'){
            i ++;
            read_inc();
        }
        return read_make_token(i);
    } else {
        while(c != '\0' && !is_white_space(c) && !is_special_char(c)) {
            read_inc();
            c = read_curr();
            i ++;
        }
        return read_make_token(i);
    }
    return NULL;
}

struct Object* read_next_token() {
    struct Object* result;
    while(1) {
        result = read_emit_token();
        if(result != NULL) {
            break;
        } else if(end_of_input) {
            printf("end of input string\n");
            return NULL;
        }
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

int read_list(struct Object*);

int read_form(struct Object* tree) {
    struct Object* token = read_next_token();
    if(token == NULL) {
        return 1;
    } else if(object_equals_char(token, '(')) {
        int error = read_list(tree);
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

        read_form(list);
        list_append_object(tree, list);
        return 0;
    } else {
        struct Object* atom = read_make_atom(token);
        list_append_object(tree, atom);
        return 0;
    }
}

int read_list(struct Object* tree) {
    struct Object* list = list_init();
    while(1) {
        int error = read_form(list);
        if(error == ')') {
            list_append_object(tree, list);
            return 0;
        } else if(error != 0) {
            printf("error: mismatched parens\n");
            return error;
        }
    }
    return 0;
}

struct Object* read_string(char* str) {
    if(str == NULL || str[0] == '\0') {
        return NULL;
    }

    buffer = str;
    offset = 0;
    length = strlen(str) + 1;
    end_of_input = 0;
    printf("%c\n", buffer[length - 1]);

    struct Object* tree = list_init();
    read_form(tree);
    object_print_string(tree);
    printf("\n");
    if(length - offset > 1) {
        printf("error: unexpected token(s) %s\n", buffer + offset);
        return NULL;
    }
    struct List* tree_list = (struct List*) tree->data.ptr;

    return tree_list->obj;
}

struct Object* read_module(char* filename) {
    file = fopen(filename, "r");
    if(file == NULL) {
        perror("failed to read module: ");
        return NULL;
    }

    end_of_input = 0;

    buffer_a = malloc(sizeof(char) * BUFFER_SIZE);
    buffer_b = malloc(sizeof(char) * BUFFER_SIZE);
    buffer = buffer_a;
    end_of_file = 0;

    length = fread(buffer, sizeof(char), BUFFER_SIZE - 1, file);
    buffer[length] = '\0';

    struct Object* tree = list_init();
    gc_insert_object(tree);
    while(!end_of_input) {
        read_form(tree);
    }
    object_print_string(tree);
    printf("\n");

    fclose(file);
    free(buffer);
    file = NULL;
    return tree;
}
