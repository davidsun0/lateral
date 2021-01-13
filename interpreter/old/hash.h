#ifndef LATERAL_HASH_H
#define LATERAL_HASH_H

#include "object.h"

struct KeyValueList {
    char* key;
    struct Object* value;
    struct KeyValueList* next;
};

struct HashMap {
    int size;
    int load;
    struct KeyValueList** pairs;
};

struct KeyValueList* hashmap_kvlist_init(char*, struct Object*);

// DJB2 string hash
unsigned int hashmap_string_hash(char*);
struct HashMap* hashmap_init(int size);

void hashmap_set(struct HashMap*, char* key, struct Object* value);
struct Object* hashmap_get(struct HashMap*, char* key);

void hashmap_free_map(struct HashMap*);

void hashmap_debug(struct HashMap*);
#endif
