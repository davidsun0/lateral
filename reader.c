#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "reader.h"

int isWhiteSpace(char c){
    if(c == ' ' || c == '\t' || c == '\n' || c == '\r') {
        return 1;
    } else {
        return 0;
    }
}

int isSpecialChar(char c){
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

void tokenize(char* str) {
    int i = 0;
    while(str[i] != '\0') {
        if(isWhiteSpace(str[i])) {
            // ignore whitespace
            i ++;
        } else if(str[i] == '~' && str[i + 1] == '@'){
            // capture ~@
            printf("~@\n");
            i += 2;
        } else if(isSpecialChar(str[i])) {
            // capture single special character
            printf("%c\n", str[i]);
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
                printf("error: unclosed quote in %.*s\n", j, str + i);
            } else {
                // capture ending quote
                j ++;
            }
            printf("%.*s\n", j, str + i);
            i += j;
        } else if(str[i] == ';') { 
            // captures all text following ; until a new line
            int j = 1;
            while(str[i + j] != '\n' && str[i + j] != '\0') {
                j ++;
            }
            printf("%.*s\n", j, str + i);
            i += j;
        } else {
            // capture regular symbols
            int j = 1;
            while(!isWhiteSpace(str[i + j]) && !isSpecialChar(str[i + j]) &&
                    str[i + j] != '\0') {
                j ++;
            }
            printf("%.*s\n", j, str + i);
            i += j;
        }
    }
}

void readString(char* str) {
    if(str == NULL) {
        return;
    }
    tokenize(str);
}
