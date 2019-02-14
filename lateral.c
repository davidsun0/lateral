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

static int lat_rep() {
    char* input_str = readline("user> ");
    if(input_str == NULL){
        return 0;
    } else if(strcmp(input_str, "") == 0) {
        return 1;
    }

    // READ
    struct Object* input = read_string(input_str);
    // error parsing, return 1 to read again
    if(input == NULL) {
        return 1;
    }

    // EVALUATE
    struct Object* output = eval_apply(user_env, input);

    // PRINT
    object_print_string(output);
    printf("\n");
    free(input_str);
    // gc_run();
    return 1;
}

static void initialize_readline(){
    // turn off tab completion
    rl_bind_key('\t', rl_insert);
}

int main(){
    initialize_readline();
    int gc_base;
    gc_init(&gc_base);
    env_init();

    while(lat_rep()){
        ;
    }
    printf("\ngoodbye! ('v' )/\n");
}
