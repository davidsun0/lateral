#ifndef LATERAL_LANG_H
#define LATERAL_LANG_H

extern struct Envir* global_env;
extern struct Envir* user_env;

// lisp truth object
extern struct Object* true_obj;
// lisp false / null object
extern struct Object* nil_obj;

void env_init();

#endif
