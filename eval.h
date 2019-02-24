#ifndef LATERAL_EVAL_H
#define LATERAL_EVAL_H

#include "object.h"
#include "hash.h"
#include "env.h"

struct StackFrame {
    struct StackFrame* prev;
    struct Object* fn;
    struct Object* expr;
    struct Object* result;
};

extern struct StackFrame* stack;

struct Object* lat_evaluate(struct Envir*, struct Object*);

#endif
