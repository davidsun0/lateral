#ifndef LA_HASH_H
#define LA_HASH_H

#include "object.h"

unsigned int str_hash(char *);

typedef struct {
    Object *buckets;
    int capacity;
    int load;
} HashMap;

HashMap *hashmap_init(int size);
void hashmap_free(HashMap *);
void hashmap_resize(HashMap *);

void hashmap_set(HashMap *, Object *key, Object *value);
Object *hashmap_get(HashMap *, Object *key);

void hashmap_debug(HashMap *);

typedef struct Envir {
    HashMap *map;
    struct Envir *inner;
    struct Envir *outer;
} Envir;

Envir *envir_init(int size);
void envir_free(Envir *);

void envir_set(Envir *, Object *key, Object *value);
void envir_set_str(Envir *, char *key, Object *value);
Object *envir_get(Envir *, Object *key);
Object *envir_search(Envir *, Object *key);

#endif
