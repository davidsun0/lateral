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
    lang_init();

    if(argc == 2) {
        // FILE INTERPRETER
        FILE *f = fopen(argv[1], "r");
        char *buffer = NULL;
        int length;
        if(f != NULL) {
            fseek(f, 0, SEEK_END);
            length = ftell(f);
            fseek(f, 0, SEEK_SET);
            buffer = malloc(sizeof(char) * (length + 1));
            if(buffer == NULL) {
                printf("failed to alloc buffer for file\n");
                exit(1);
            }
            fread(buffer, 1, length, f);
            buffer[length] = '\0';
        } else {
            printf("failed to open file %s\n", argv[1]);
            exit(1);
        }

        Object *tokens = NULL; 
        Object *curr = tokens;
        Object *obj = NULL;
        char *lbuf = buffer;
        while(read_token(&lbuf, &obj) >= 0) {
            curr = list_append(curr, obj);
            if(tokens == NULL) {
                tokens = curr;
            }
        }
        fclose(f);
        free(buffer);

        obj_print(tokens, 0);
        printf("\n");
        // insert program into environment to avoid garbage collection
        Object *prog = obj_init_str(strt, "*PROG*");
        envir_set(curr_envir, prog, tokens);

        // list_debug0(tokens, 0);
        Object *ast = NULL;
        curr = tokens;
        // obj_print(tokens, 0);
        // printf("\n");
        while(curr != nil_obj) {
            read_form(&curr, &ast);
            // obj_print(ast, 0);
            // printf("\n");
            evaluate(curr_envir, ast);
            garbage_run();
        }
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
