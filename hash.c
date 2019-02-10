#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "hash.h"

unsigned int hashmap_string_hash(char* str) {
    unsigned int hash = 5381;
    while(*str != '\0') {
        hash = hash * 33 + *str;
        str ++;
    }
    return hash;
}

struct HashMap* hashmap_init(int size) {
    struct HashMap* map = malloc(sizeof(struct HashMap));
    map->size = size;
    map->load = 0;
    map->pairs = malloc(sizeof(struct KeyValue*) * size);
    for(int i = 0; i < size; i ++) {
        map->pairs[i] = NULL;
    }
    return map;
}

struct KeyValueList* hashmap_kvlist_init(char* key, struct Object* value) {
    struct KeyValueList* kvlist = malloc(sizeof(struct KeyValueList));
    char* key_copy = malloc(sizeof(char) * (strlen(key) + 1));
    strcpy(key_copy, key);
    kvlist->next = NULL;
    kvlist->key = key_copy;
    kvlist->value = value;
    return kvlist;
}

void hashmap_free_map(struct HashMap* map) {
    for(int i = 0; i < map->size; i ++) {
        if(map->pairs[i] != NULL) {
            struct KeyValueList* list = map->pairs[i];
            struct KeyValueList* prev = list;
            while(list != NULL) {
                prev = list;
                list = list->next;
                free(prev->key);
                free(prev);
            }
        }
    }
    free(map->pairs);
    free(map);
}

void hashmap_double_size(struct HashMap* map) {
    printf("resizing not implemented");
    printf("load: %d\tcapacity: %d\n", map->load, map->size);
    /*
    // TODO: rewrite to only make new array of lists
    struct HashMap* new_map = hashmap_init(map->size * 2);
    for(int i = 0; i < map->size; i ++) {
        if(map->pairs[i] != NULL) {
            struct KeyValueList* list = map->pairs[i];
            while(list != NULL) {
                hashmap_set(new_map, list->keyValue.key, list->keyValue.value);
                list = list->next;
            }
        }
    }
    hashmap_free_map(map);
    */
}

void hashmap_set(struct HashMap* map, char* key, struct Object* value) {
    unsigned int hash = hashmap_string_hash(key) % map->size;
    if((float)(map->load + 1)/map->size > 0.7) {
        printf("resizing hashmap\n");
        hashmap_double_size(map);
        hash = hashmap_string_hash(key) % map->size;
    }

    if(map->pairs[hash] == NULL) {
        // init new linked list and set object
        struct KeyValueList* kvlist = hashmap_kvlist_init(key, value);
        map->pairs[hash] = kvlist;
        map->load ++;
    } else {
        struct KeyValueList* list = map->pairs[hash];
        struct KeyValueList* prev;
        int replaced = 0;
        while(list != NULL) {
            // replace existing value
            if(strcmp(key, list->key) == 0) {
                list->value = value;
                replaced = 1;
                break;
            }
            prev = list;
            list = list->next;
        }
        // append to list
        if(!replaced) {
            prev->next = hashmap_kvlist_init(key, value);
            map->load ++;
        }
    }
}

struct Object* hashmap_get(struct HashMap* map, char* key) {
    int hash = hashmap_string_hash(key) % map->size;
    if(map->pairs[hash] == NULL) {
        return NULL;
    } else {
        struct KeyValueList* list = map->pairs[hash];
        while(list != NULL) {
            if(strcmp(key, list->key) == 0) {
                return list->value;
            }
            list = list->next;
        }
        return NULL;
    }
}

void hashmap_debug(struct HashMap* map) {
    for(int i = 0; i < map->size; i ++) {
        if(map->pairs[i] != NULL) {
            struct KeyValueList* kvlist = map->pairs[i];
            while(kvlist != NULL) {
                printf("key: %s\nvalue: ", kvlist->key);
                object_debug(kvlist->value, 0);
                printf("\n");
                kvlist = kvlist->next;
            }
        }
    }
}
