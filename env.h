#ifndef LATERAL_ENV_H
#define LATERAL_ENV_H

struct Envir {
    struct HashMap* map;
    struct Envir* inner;
    struct Envir* outer;
};

struct Envir* envir_init(int size);
void envir_free(struct Envir*);

void envir_push(struct Envir*, struct Envir*);
struct Envir* envir_pop(struct Envir*);

void envir_set(struct Envir*, char*, struct Object*);
struct Object* envir_get(struct Envir*, char*);
struct Object* envir_search(struct Envir*, char*);

#endif
