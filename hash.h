#ifndef LATERAL_HASH_H
#define LATERAL_HASH_H

#include "object.h"

struct KeyValue {
    char* key;
    struct Object* value;
};

struct KeyValueList {
    struct KeyValue keyValue;
    struct KeyValueList* next;
};

struct HashMap {
    int size;
    int load;
    struct KeyValueList** pairs;
};

int hashmap_string_hash(char*);
struct HashMap* hashmap_init(int size);

struct HashMap* hashmap_double_size(struct HashMap*);
void hashmap_set(struct HashMap*, char* key, struct Object*);
struct Object* hashmap_get(struct HashMap*, char* key);

void hashmap_free_map(struct HashMap*);
#endif
