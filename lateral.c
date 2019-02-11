#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "object.h"
#include "garbage.h"
#include "env.h"
#include "reader.h"
#include "eval.h"
#include "lang.h"

extern struct Envir* user_env;
extern int object_count;

int lat_rep() {
    char* input_str = readline("user> ");
    if(input_str == NULL){
        return 0;
    } else if(strcmp(input_str, "") == 0) {
        return 1;
    }

    struct Object* input = read_string(input_str);
    // error parsing, return to read again
    if(input == NULL) {
        return 0;
    }

    struct Object* output = eval_apply(user_env, input);
    object_print_string(output);
    printf("\n");
    // printf("objects: %d\n", object_count);
    free(input_str);
    gc_run();
    return 1;
}

void initialize_readline(){
    // turn off tab completion
    rl_bind_key('\t', rl_insert);
}

int main(){
    initialize_readline();
    gc_init();
    env_init();

    while(lat_rep()){
        ;
    }
    printf("\ngoodbye! ('v' )/\n");
}
