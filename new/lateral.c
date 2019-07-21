#include <stdio.h>
#include <stdlib.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "object.h"
#include "reader.h"
#include "eval.h"
#include "core.h"

Object *la_eval(Object *ast) {
    return ast;
}

void la_print();

int main() {
    // turn off tab completion
    rl_bind_key('\t', rl_insert);

    curr_envir = envir_init(32);
    lang_init();

    while(1) {
        char *input_str = readline("user> ");
        if(input_str == NULL) {
            break;
        } else if(input_str[0] == '\0') {
            free(input_str);
            continue;
        }

        // read
        Object* ast = read_string(input_str);
        free(input_str);
        if(ast == NULL) {
            continue;
        }

        // eval
        Object *result = evaluate(curr_envir, ast);

        // print
        if(result == NULL) {
            printf("NULL RESULT\n");
        } else {
            obj_print(result);
            printf("\n");
        }
    }

    printf("\ngoodbye! ('u' )/\n");
}
