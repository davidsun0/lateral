#include <stdlib.h>
#include <stdio.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "object.h"
#include "env.h"
#include "reader.h"
#include "eval.h"
#include "printer.h"

extern struct HashMap* envir;

struct Object* lat_read() {
    char* str = readline("user> ");
    return read_string(str);
}

struct Object* lat_eval(struct Object* obj) {
    return eval_apply(envir, eval_eval(envir, obj));
}

void lat_print(struct Object* obj) {
    print_string(obj);
    printf("\n");
    // object_print_debug(obj);
}

int lat_rep() {
    struct Object* input = lat_read();
    if(input == NULL){
        printf("\ngoodbye! (^_^ )/\n");
        return 0;
    }

    struct Object* output = lat_eval(input);
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
    env_init();

    while(lat_rep()){
        ;
    }
}
