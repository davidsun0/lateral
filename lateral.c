#include <stdlib.h>
#include <stdio.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "reader.h"

char* lat_read() {
    char* str = readline("user> ");
    read_string(str);
    return str;
}

char* lat_eval(char* str) {
    return str;
}

void lat_print(char* str) {
    printf("%s\n", str);
}

int lat_rep() {
    char* input = lat_read();
    if(input == NULL){
        printf("\ngoodbye! (^_^ )/\n");
        return 0;
    }

    char* output = lat_eval(input);
    lat_print(output);
    free(output);
    return 1;
}

void initialize_readline(){
    // turn off tab completion
    rl_bind_key('\t', rl_insert);
}

int main(){
    initialize_readline();

    while(lat_rep()){
        ;
    }
}
