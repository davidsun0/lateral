#ifndef LA_EVAL_H
#define LA_EVAL_H

#include "object.h"

enum mode {
    eval_exe,
    apply_exe,
    macro_exe,
    result_exe,
    unknown
};

typedef struct StackFrame {
    struct StackFrame* prev;
    Object *in;
    Object *out;
    Object *ret;
    enum mode exe_mode;
} StackFrame;

StackFrame *stack;
Envir *curr_envir;
Envir *user_envir;

Envir *envir_push(Envir*, Object *params, Object *args);
Envir *envir_pop(Envir*);

Object *evaluate(Envir *envir, Object *ast);

Object *funcall2(Object *fn, Object *args);
Envir *envir_push2(Object* params, Object* args);
void envir_pop2();

#endif
