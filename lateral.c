#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "object.h"
#include "env.h"
#include "reader.h"
#include "eval.h"

extern struct HashMap* envir;

struct Object* lat_eval(struct Object* obj) {
    return eval_apply(envir, eval_eval(envir, obj));
}

int lat_rep() {
    char* input_str = readline("user> ");
    if(input_str == NULL){
        printf("\ngoodbye! (^_^ )/\n");
        return 0;
    } else if(strcmp(input_str, "") == 0) {
        return 1;
    }

    struct Object* input = read_string(input_str);
    // error parsing, return to read again
    if(input == NULL) {
        return 0;
    }

    struct Object* output = eval_apply(envir, input);
    //lat_eval(input);
    // lat_print(output);
    object_print_string(output);
    printf("\n");
    return 1;
}

void initialize_readline(){
    // turn off tab completion
    rl_bind_key('\t', rl_insert);
}

int main(){
    initialize_readline();
    env_init();

    while(lat_rep()){
        ;
    }
}
