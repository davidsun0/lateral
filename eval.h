#ifndef LATERAL_EVAL_H
#define LATERAL_EVAL_H

#include "object.h"
#include "hash.h"

struct Object* eval_eval(struct HashMap*, struct Object*);
struct Object* eval_apply(struct HashMap*, struct Object*);

#endif
