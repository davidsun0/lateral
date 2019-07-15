#ifndef LA_EVAL_H
#define LA_EVAL_H

#include "object.h"
#include "list.h"
#include "hash.h"

enum mode {
    eval_exe,
    apply_exe,
    result_exe,
    unknown
};

typedef struct StackFrame {
    struct StackFrame* prev;
    Object *fn;
    Object *in;
    List *in_list;
    Object *out;
    List *out_list;
    Object *ret;
    enum mode exe_mode;
} StackFrame;

StackFrame *stack;
Envir *curr_envir;

Object *evaluate(Envir *envir, Object *ast);

#endif
