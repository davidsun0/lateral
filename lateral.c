#include <stdlib.h>
#include <stdio.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "reader.h"

char* lread() {
    char* str = readline("user> ");
    readString(str);
    return str;
}

char* leval(char* str) {
    return str;
}

void lprint(char* str) {
    printf("%s\n", str);
}

int rep() {
    char* input = lread();
    if(input == NULL){
        printf("\ngoodbye! (^_^ )/\n");
        return 0;
    }

    char* output = leval(input);
    lprint(output);
    free(output);
    return 1;
}

void initializeReadline(){
    // turn off tab completion
    rl_bind_key('\t', rl_insert);
}

int main(){
    initializeReadline();

    while(rep()){
        ;
    }
}
