#include <stdio.h>
#include <stdlib.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "object.h"
#include "reader.h"
#include "eval.h"
#include "core.h"
#include "garbage.h"

void la_repl() {
}

int main(int argc, char ** argv) {
    garbage_init();
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
        }

        List *tokens = list_init();
        Object *obj = NULL;
        List *curr = tokens;
        char *lbuf = buffer;
        while(read_token(&lbuf, &obj) >= 0) {
            curr = list_append(curr, obj);
        }
        fclose(f);
        free(buffer);

        union Data dat = { .ptr = tokens };
        Object *token_wrapper = obj_init(listt, dat);
        char *prog = la_strdup("*PROG*");
        union Data dat2 = { .ptr = prog };
        Object *program = obj_init(symt, dat2);
        envir_set(curr_envir, program, token_wrapper);

        // list_debug0(tokens, 0);
        Object *ast = NULL;
        curr = tokens;
        while(curr != NULL) {
            read_form(&curr, &ast);
            Object *ret = evaluate(curr_envir, ast);
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
    }
}
