#ifndef LATERAL_EVAL_H
#define LATERAL_EVAL_H

#include "object.h"
#include "hash.h"
#include "env.h"

enum mode {
    eval_exe,
    apply_exe,
    macro_exe,
    result_exe,
    unknown
};

struct StackFrame {
    struct StackFrame* prev;
    struct Object* fn;
    struct Object* expr;
    struct Object* working;
    struct Object* ret;
    enum mode exe_mode;
    int eval_index;
};

extern struct StackFrame* stack;
struct Envir* test;

void stack_print();
struct Object* lat_evaluate(struct Envir*, struct Object*);

#endif
