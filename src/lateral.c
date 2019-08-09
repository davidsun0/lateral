#include <stdio.h>
#include <stdlib.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "object.h"
#include "reader.h"
#include "eval.h"
#include "core.h"
#include "garbage.h"

int main(int argc, char ** argv) {
    garbage_init();

    // NIL is a cons cell with CAR and CDR as nil
    union Data ndat = { .cell = {NULL, NULL}};
    nil_obj = obj_init(listt, ndat);
    CAR(nil_obj) = nil_obj;
    CDR(nil_obj) = nil_obj;

    curr_envir = envir_init(32);
    user_envir = curr_envir;
    lang_init();

    read_file("./core.lisp");
    if(argc == 2) {
        read_file(argv[1]);
    } else {
        //REPL MODE
        
        // turn off tab completion
        rl_bind_key('\t', rl_insert);

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
                obj_print(result, 0);
                printf("\n");
            }
            garbage_run();
        }

        printf("\ngoodbye! ('u' )/\n");

        envir_free(curr_envir);
        // garbage_shutdown();
    }
}
