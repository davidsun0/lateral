#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "object.h"
#include "list.h"
#include "garbage.h"
#include "error.h"

#include "reader.h"

#define is_white_space(c) ((c) == ' ' || (c) == '\n' || (c) == ',' || \
        (c) == '\t' || (c) == '\r')

#define is_special_char(c) ((c) == '(' || (c) == ')' || (c) == '[' || \
        (c) == ']' || (c) == '{' || (c) == '}' || (c) == '\'' || c == '`' || \
        (c) == '^' || (c) == '~')

#define BUFFER_SIZE 256

static FILE* file;
static char* buffer;
static char* buffer_a;
static char* buffer_b;
static int offset;
static int length;
static int end_of_input;
static int end_of_file;

/**
 * Creates a token containing the last len characters of the read buffer.
 *
 * @param len   number of chars to make a symbol from
 * @return      an object of char_type or string containing a copy of its data
 */
static struct Object* read_make_token(int len) {
    enum object_type type;
    union Data data;

    if(len == 1) {
        type = char_type;
        if(offset != 0) {
            data.char_type = buffer[offset - 1];
        } else {
            // char exists in old buffer
            char* old_buffer = buffer == buffer_a ? buffer_b : buffer_a;
            // remember that old_buffer[BUFFER_SIZE - 1] == '\0'
            data.char_type = old_buffer[BUFFER_SIZE - 2];
        }
    } else {
        type = string;
        char* sym = malloc(sizeof(char) * (len + 1));
        if(offset - len < 0) {
            // token extends across buffer boundary
            int runover = offset - len;
            char* old_buffer = buffer == buffer_a ? buffer_b : buffer_a;
            strncpy(sym, old_buffer + BUFFER_SIZE + runover - 1, -runover);
            strncpy(sym - runover, buffer, offset);
            // TODO: verify this index of len + 1
            printf("%s \n", sym);
            sym[len + 1] = '\0';
            data.ptr = sym;
        } else {
            strncpy(sym, buffer + offset - len, len);
            sym[len] = '\0';
            data.ptr = sym;
        }
    }
    struct Object* output = object_init(type, data);
    return output;
}

/**
 * Increments the char pointer in the buffer.
 * Reads from file and swaps buffers if necessary and sets end_of_input to 1
 * upon EOF.
 */
static void read_inc() {
    if(offset < length - 2) {
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

/**
 * Creates the next token from the input buffer.
 *
 * @return  object containing token or NULL when whitespace or comments are read
 */
static struct Object* read_emit_token() {
    char c = buffer[offset];
    int chars_read = 0;
    if(c == '\0') {
        return NULL;
    } else if(is_special_char(c)) {
        // special chars
        read_inc();
        chars_read ++;
        if(c == '~' && buffer[offset] == '@') {
            read_inc();
            chars_read ++;
        }
        return read_make_token(chars_read);
    } else if(is_white_space(c)) {
        // white space
        while(c != '\0' && is_white_space(c)) {
            read_inc();
            c = buffer[offset];
        }
        return NULL;
    } else if(c == ';') {
        // comments
        while(c != '\0' && c != '\n') {
            read_inc();
            c = buffer[offset];
        }
        return NULL;
    } else if(c == '"') {
        // strings
        read_inc();
        char prev = c;
        // read first " character
        c = buffer[offset];
        chars_read ++;
        while(c != '\0') {
            if(c == '"' && prev != '\\') {
                break;
            }
            read_inc();
            prev = c;
            c = buffer[offset];
            chars_read ++;
        }
        if(c == '"'){
            chars_read ++;
            read_inc();
        }
        return read_make_token(chars_read);
    } else {
        // symbols
        while(c != '\0' && !is_white_space(c) && !is_special_char(c)) {
            read_inc();
            c = buffer[offset];
            chars_read ++;
        }
        return read_make_token(chars_read);
    }
}

/**
 * Reads the next valid token in the buffer.
 * Returns NULL when EOF is met and there are no more tokens to be read.
 *
 * @return  a char_type or string object with a seperate copy of its contents
 */
static struct Object* read_next_token() {
    struct Object* result;
    while(1) {
        result = read_emit_token();
        if(result != NULL) {
            break;
        } else if(end_of_input) {
            return NULL;
        }
    }
    return result;
}

struct Object* parse_number(struct Object* str) {
    if(str->type == char_type) {
        union Data data;
        data.int_type = str->data.char_type - '0';
        return object_init(int_type, data);
    } else if(str->type != string) {
        printf("error: can only parse number from char or string\n");
        return error_init();
    }

    char* dat = str->data.ptr;
    union Data data;
    // parse integer
    int int_value = 0;
    int negative = 0;
    if(dat[0] == '-') {
        negative = 1;
        dat ++;
    }
    while(*dat != '\0') {
        if(*dat == '.') {
            float float_value = int_value;
            dat ++;
            float power = 0.1;
            while('0' <= *dat && *dat <= '9') {
                float_value += power * (*dat - '0');
                dat ++;
                power = power / 10.0;
            }
            if(*dat != '\0') {
                printf("error: failed to parse number\n");
                return error_init();
            }
            if(negative)
                float_value *= -1;
            data.float_type = float_value;
            return object_init(float_type, data);
        } else if(*dat < '0' || '9' < *dat) {
            printf("error: failed to parse integer\n");
            return error_init();
        }

        int_value *= 10;
        int_value += *dat - '0';
        dat ++;
    }

    if(negative)
        int_value *= -1;
    data.int_type = int_value;
    return object_init(int_type, data);
}

/**
 * Creates a new object from a token parsed as the appropriate datatype.
 *
 * @return  the parsed version of the input token
 */
static struct Object* read_make_atom(struct Object* obj) {
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
                    case 't':
                        str[j] = '\t';
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
        return parse_number(obj);
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

static int read_list(struct Object* tree);

/**
 * Reads the next object from input and appends it to the syntax tree.
 * Mutually recursive with read_list.
 *
 * @param tree  working syntax tree
 * @return      1 upon EOF; ')' upon end of list; 0 otherwise
 */
static int read_form(struct Object* tree) {
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

/**
 * Reads the next list from input and appends it to the syntax tree.
 * Mutually recursive with read_form
 *
 * @param tree  working syntax tree
 * @return      1 on EOF; 0 on success
 */
static int read_list(struct Object* tree) {
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

/**
 * Builds a syntax tree from the input string.
 *
 * @param str   null terminated ASCII to parse
 * @return      corresponding lisp syntax tree
 */
struct Object* read_string(char* str) {
    if(str == NULL || str[0] == '\0') {
        return NULL;
    }

    buffer = str;
    offset = 0;
    length = strlen(str) + 1;
    end_of_input = 0;

    struct Object* tree = list_init();
    read_form(tree);
    if(length - offset > 1) {
        printf("error: unexpected token(s) %s\n", buffer + offset);
        return error_init();
    }
    struct List* tree_list = (struct List*) tree->data.ptr;

    return tree_list->obj;
}

/**
 * Builds a syntax tree from text file.
 *
 * @param filename  path to a plain text file containing lisp code
 * @return          corresponding lisp syntax tree
 */
struct Object* read_module(char* filename) {
    file = fopen(filename, "r");
    if(file == NULL) {
        perror("failed to open module: ");
        return NULL;
    }

    // allocate temporary buffers
    buffer_a = malloc(sizeof(char) * BUFFER_SIZE);
    buffer_b = malloc(sizeof(char) * BUFFER_SIZE);

    // set up global variables for parsing
    buffer = buffer_a;
    end_of_file = 0;
    end_of_input = 0;

    // initial read from file
    length = fread(buffer, sizeof(char), BUFFER_SIZE - 1, file);
    buffer[length] = '\0';

    struct Object* tree = list_init();
    while(!end_of_input) {
        read_form(tree);
    }

    free(buffer_a);
    free(buffer_b);
    fclose(file);
    file = NULL;
    return tree;
}
