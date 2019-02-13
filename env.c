#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "hash.h"
#include "garbage.h"

#include "env.h"

struct Envir* envir_init(int size) {
    struct Envir* envir = gc_malloc(sizeof(struct Envir));
    envir->map = hashmap_init(size);
    envir->inner = NULL;
    envir->outer = NULL;
    return envir;
}

void envir_free(struct Envir* envir) {
    hashmap_free_map(envir->map);
    free(envir);
}

void envir_set(struct Envir* envir, char* key, struct Object* value) {
    hashmap_set(envir->map, key, value);
}

struct Object* envir_get(struct Envir* envir, char* key) {
    return hashmap_get(envir->map, key);
}

struct Object* envir_search(struct Envir* envir, char* key) {
    struct Object* value = NULL;
    while(value == NULL && envir != NULL) {
        value = envir_get(envir, key);
        envir = envir->outer;
    }
    return value;
}
