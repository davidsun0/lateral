#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"

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
    HashMap *map = malloc(sizeof(HashMap));
    map->capacity = size;
    map->load = 0;
    map->buckets = malloc(sizeof(List) * size);
    for(int i = 0; i < size; i ++) {
        map->buckets[i].obj = NULL;
        map->buckets[i].next = NULL;
    }
    return map;
}

void hashmap_free(HashMap *map) {
    for(int i = 0; i < map->capacity; i ++) {
        if(map->buckets[i].next != NULL) {
            list_free(map->buckets[i].next);
        }
    }
    free(map->buckets);
    free(map);
}

#define list_key(list) (((List *)((list)->obj->data.ptr))->obj)
#define list_value(list) (((List *)((list)->obj->data.ptr))->next->obj)

void hashmap_resize(HashMap *map) {
    HashMap *newmap = hashmap_init(map->capacity * 2);
    for(int i = 0; i < map->capacity; i ++) {
        List *list = map->buckets + i;
        while(list != NULL) {
            if(list->obj->type != listt) {
                printf("error: hashmap corrupted\n");
                exit(1);
            }
            List *keyval = (List *)list->obj->data.ptr;
            if(keyval->obj == NULL || keyval->next == NULL || keyval->next->obj == NULL) {
                printf("error: hashmap corrupted\n");
                exit(1);
            }
            Object *key = keyval->obj;
            Object *val = keyval->next->obj;
            hashmap_set(newmap, key, val);

            list = list->next;
        }
    }
    list_free(map->buckets);
    map->buckets = newmap->buckets;
    map->capacity = newmap->capacity;
    free(newmap);
}

void hashmap_set(HashMap *map, Object *key, Object *value) {
    if((float)map->load / map->capacity > 0.7) {
        hashmap_resize(map);
    }

    List *keyval = list_init();
    keyval->obj = key;
    keyval->next = list_init();
    keyval->next->obj = value;
    union Data dat = { .ptr = keyval };
    Object *keyval_obj = obj_init(listt, dat);

    unsigned int hash = obj_hash(key) % map->capacity;
    if(map->buckets[hash].obj == NULL) {
        map->buckets[hash].obj = keyval_obj;
    } else {
        List *list = map->buckets + hash;
        while(list != NULL) {
            if(obj_equals(key, list_key(list))) {
                list_value(list) = value;
                return;
            }
        }

        list_append(list, keyval_obj);
        map->load ++;
    }
}

Object *hashmap_get(HashMap *map, Object *key) {
    unsigned int hash = obj_hash(key) % map->capacity;
    if(map->buckets[hash].obj == NULL) {
        return NULL;
    } else {
        List *list = &(map->buckets[hash]);
        while(list != NULL) {
            if(list->obj->type != listt) {
                printf("error: hashmap corrupted\n");
                exit(1);
            }
            List *keyval = (List *)list->obj->data.ptr;
            if(keyval->obj == NULL || keyval->next == NULL || keyval->next->obj == NULL) {
                printf("error: hashmap corrupted\n");
                exit(1);
            }
            Object *tkey = keyval->obj;

            if(obj_equals(key, tkey)) {
                Object *val = keyval->next->obj;
                return val;
            } else {
                list = list->next;
            }
        }
        return NULL;
    }
    return NULL;
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
