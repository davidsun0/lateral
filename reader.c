#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "list.h"

#include "reader.h"

int is_white_space(char c){
    if(c == ' ' || c == '\t' || c == '\n' || c == '\r') {
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
    char* sym = malloc(sizeof(char) * (len + 1));
    strncpy(sym, str, len);
    sym[len] = '\0';
    return list_append(list, sym);
}

void read_tokenize(char* str) {
    struct List* list = malloc(sizeof(struct List));
    list->data = NULL;
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

    list_print(list);
    // TODO: free input string
}

void read_string(char* str) {
    if(str == NULL) {
        return;
    }
    read_tokenize(str);
}

