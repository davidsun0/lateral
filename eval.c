#include <stdlib.h>
#include <stdio.h>

#include "lang.h"
#include "list.h"

#include "eval.h"

struct StackFrame* stack;
struct Envir* curr_env;

void push_stack(struct Object* ast) {
    struct StackFrame* new_frame = malloc(sizeof(struct StackFrame));
    new_frame->prev = stack;
    new_frame->fn = NULL;
    new_frame->expr = ast;
    new_frame->result = NULL;
    stack = new_frame;
}

struct Object* pop_stack() {
    if(stack == NULL) {
        printf("ERROR: popping an empty stack\n");
        return NULL;
    }

    struct StackFrame* old_frame = stack;
    struct Object* result = old_frame->result;
    stack = stack->prev;
    free(old_frame);
    return result;
}

void print_stack() {
    struct StackFrame* frame = stack;

    while(frame != NULL) {
        printf("\n=== STACK FRAME ===\n");
        printf("fn:   ");
        object_print_string(frame->fn);
        printf("\nexpr: ");
        object_print_string(frame->expr);
        printf("\nres:  ");
        object_print_string(frame->result);
        printf("\nnext frame: %p\n\n", (void*) frame->prev);
        frame = frame->prev;
    }
}

static void eval_apply();

static void eval_eval() {
    struct Object* ast = stack->expr;
    if(ast == NULL) {
        stack->result = NULL;
        return;
    }

    if(ast->type == list_type) {
        struct List* sexpr = (struct List*) ast->data.ptr;
        stack->result = list_init();
        struct Object* result = stack->result;
        while(sexpr != NULL) {
            push_stack(sexpr->obj);
            eval_apply();
            struct Object* obj = pop_stack();
            list_append_object(result, obj);
            sexpr = sexpr->next;
        }
        object_print_string(stack->result);
        printf("\n");
    } else if(ast->type == symbol) {
       stack->result = envir_search(curr_env, ast->data.ptr);
       // TODO: error handling
       if(stack->result == NULL) {
           printf("error: symbol %s not found\n", (char*) ast->data.ptr);
       }
    } else {
        stack->result = ast;
    }
}

static void eval_apply() {
    struct Object* ast = stack->expr;
    if(ast == NULL) {
        stack->result = NULL;
        return;
    }

    if(ast->type != list_type) {
        push_stack(ast);
        eval_eval();
        struct Object* result = pop_stack();
        stack->result = result;
    } else {
        struct List* sexpr = (struct List*) ast->data.ptr;
        if(list_is_empty(sexpr)) {
            stack->result = nil_obj;
            return;
        } else {
            stack->fn = sexpr->obj;
        }

        push_stack(ast);
        eval_eval();
        ast = pop_stack();
        stack->result = ast;

        // not a list after evaluation
        // i.e. evaluated by a special form
        if(ast == NULL || ast->type != list_type) {
            stack->result = ast;
            return;
        }

        sexpr = (struct List*) ast->data.ptr;
        struct Object* func = sexpr->obj;
        sexpr = sexpr->next;

        if(func->type == c_fn) {
            print_stack();
        }
    }
}

struct Object* lat_evaluate(struct Envir* envir, struct Object* ast) {
    curr_env = envir;
    push_stack(ast);
    eval_apply();
    struct Object* result = pop_stack();
    if(stack != NULL) {
        print_stack();
    }
    return result;
}
