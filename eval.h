#ifndef LATERAL_EVAL_H
#define LATERAL_EVAL_H

#include "object.h"
#include "hash.h"
#include "env.h"

#define eval_type 0
#define apply_type 1

struct StackFrame {
    struct StackFrame* prev;
    struct Object* fn;
    struct Object* expr;
    struct Object* working;
    struct Object* ret;
    int exe_mode;
    int eval_index;
};

extern struct StackFrame* stack;

void stack_print();
struct Object* lat_evaluate(struct Envir*, struct Object*);

#endif
