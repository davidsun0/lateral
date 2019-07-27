#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "reader.h"

#include "hash.h"

unsigned int str_hash(char *str) {
    unsigned int hash = 5381;
    int i = 0;
    while(str[i] != '\0') {
        hash += str[i] * 33;
        i ++;
    }
    return hash;
}

HashMap *hashmap_init(int size) {
    if(size < 1)
        size = 1;
    HashMap *map = malloc(sizeof(HashMap));
    map->capacity = size;
    map->load = 0;

    map->buckets = malloc(sizeof(Object) * size);
    for(int i = 0; i < size; i ++) {
        (map->buckets + i)->type = listt;
        CAR(map->buckets + i) = nil_obj;
        CDR(map->buckets + i) = nil_obj;
    }
    return map;
}

void hashmap_free(HashMap *map) {
    free(map->buckets);
    free(map);
}

void hashmap_resize(HashMap *map) {
    printf("warning: hashmap_resize has not been tested\n");
    HashMap *newmap = hashmap_init(map->capacity * 2);
    for(int i = 0; i < map->capacity; i ++) {
        Object *list = map->buckets + i;
        while(list != nil_obj) {
            Object *keyval = CAR(list);
            Object *key = CAR(keyval);
            Object *val = CDR(keyval);
            hashmap_set(newmap, key, val);

            list = CDR(list);
        }
    }
    free(map->buckets);
    map->buckets = newmap->buckets;
    map->capacity = newmap->capacity;
    free(newmap);
}

void hashmap_set(HashMap *map, Object *key, Object *value) {
    if((float)map->load / map->capacity > 0.7) {
        hashmap_resize(map);
    }

    unsigned int hash = obj_hash(key) % map->capacity;
    if(CAR(map->buckets + hash) == nil_obj) {
        Object *keyval = cell_init();
        CAR(keyval) = key;
        CDR(keyval) = value;

        CAR(map->buckets + hash) = keyval;
        map->load ++;
    } else {
        Object *list = map->buckets + hash;
        while(list != nil_obj) {
            if(obj_equals(key, CAR(CAR(list)))) {
                // update value
                CDR(CAR(list)) = value;
                return;
            } else if(CDR(list) == nil_obj) {
                // append to end of linked list
                Object *keyval = cell_init();
                CAR(keyval) = key;
                CDR(keyval) = value;

                Object *bucket = cell_init();
                CAR(bucket) = keyval;
                CDR(list) = bucket;
                map->load ++;
                return;
            }
            list = CDR(list);
        }
    }
}

Object *hashmap_get(HashMap *map, Object *key) {
    unsigned int hash = obj_hash(key) % map->capacity;
    Object *list = map->buckets + hash;
    if(CAR(list) == nil_obj) {
        return NULL;
    } else {
        while(list != nil_obj) {
            Object *keyval = CAR(list);

            if(obj_equals(key, CAR(keyval))) {
                Object *val = CDR(keyval);
                return val;
            } else {
                list = CDR(list);
            }
        }
        return NULL;
    }
    return NULL;
}

void hashmap_debug(HashMap *map) {
    for(int i = 0; i < map->capacity; i ++) {
        Object *list = map->buckets + i;
        if(CAR(list) != nil_obj) {
            while(list != nil_obj) {
                Object *key = CAR(CAR(list));
                Object *value = CDR(CAR(list));
                printf("===\n");
                printf("key: ");
                obj_debug(key);
                printf("value: ");
                obj_debug(value);

                list = CDR(list);
            }
        }
    }
}

Envir *envir_init(int size) {
    Envir *envir = malloc(sizeof(Envir));
    envir->map = hashmap_init(size);
    envir->inner = NULL;
    envir->outer = NULL;
    return envir;
}

void envir_free(Envir *envir) {
    hashmap_free(envir->map);
    free(envir);
}

void envir_set(Envir *envir, Object *key, Object *value) {
    hashmap_set(envir->map, key, value);
}

void envir_set_str(Envir *envir, char *key, Object *value) {
    char *key_str = la_strdup(key);
    union Data dat = { .ptr = key_str };
    Object *key_obj = obj_init(symt, dat);
    hashmap_set(envir->map, key_obj, value);
}

Object *envir_get(Envir *envir, Object *key) {
    return hashmap_get(envir->map, key);
}

Object *envir_search(Envir *envir, Object *key) {
    Object *output;
    while((output = envir_get(envir, key)) == NULL) {
        envir = envir->outer;
        if(envir == NULL) {
            return NULL;
        }
    }
    return output;
}
