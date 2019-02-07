#ifndef LATERAL_EVAL_H
#define LATERAL_EVAL_H

#include "object.h"
#include "hash.h"
#include "env.h"

struct Object* eval_eval(struct Envir*, struct Object*);
struct Object* eval_apply(struct Envir*, struct Object*);

#endif
