#ifndef LATERAL_ENV_H
#define LATERAL_ENV_H

struct Envir {
    struct HashMap* map;
    struct Envir* inner;
    struct Envir* outer;
};

struct Envir* envir_init(int size);
void envir_free(struct Envir*);

// inserts a key value pair into environment hashmap
void envir_set(struct Envir*, char*, struct Object*);

// searches environment with char key
struct Object* envir_get(struct Envir*, char*);

// recursively searches outer environments with char key
struct Object* envir_search(struct Envir*, char*);

#endif
