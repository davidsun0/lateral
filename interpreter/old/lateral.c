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

static void lat_repl() {
    while(1) {
        char* input_str = readline("user> ");
        if(input_str == NULL){
            break;
        } else if(strcmp(input_str, "") == 0) {
            continue;
        }

        // READ
        struct Object* input = read_string(input_str);
        free(input_str);
        // error parsing; read again
        if(input == NULL || input->type == error_type) {
            continue;
        }

        // EVALUATE
        struct Object* output = lat_evaluate(user_env, input);

        // PRINT
        printf("=> ");
        object_print_string(output);
        printf("\n");
    }
}

static void initialize_readline(){
    // turn off tab completion
    rl_bind_key('\t', rl_insert);
}

int main(){
    initialize_readline();
    gc_init();
    env_init();

    lat_repl();
    printf("\ngoodbye! ('u' )/\n");
}
