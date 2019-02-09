#ifndef LATERAL_LANG_H
#define LATERAL_LANG_H

void env_init();

struct Object* lat_lambda(struct List*);

struct Object* true_obj;
struct Object* nil_obj;

#endif
