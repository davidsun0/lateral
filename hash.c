#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "hash.h"

int hashmap_string_hash(char* str) {
    int hash = 5381;
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

struct HashMap* hashmap_double_size(struct HashMap* map) {

}

void hashmap_set(struct HashMap* map, char* key, struct Object* value) {
    int hash = hashmap_string_hash(key) % map->size;
    if((float)(map->load + 1)/map->size > 0.7) {
        map = hashmap_double_size(map);
        hash = hashmap_string_hash(key) % map->size;
    }

    if(map->pairs[hash] == NULL) {
        // init new linked list and set object
        struct KeyValueList* list = malloc(sizeof(struct KeyValueList));
        list->next = NULL;
        list->keyValue.key = key;
        list->keyValue.value = value;
        map->pairs[hash] = list;
    } else {
        struct KeyValueList* list = map->pairs[hash];
        struct KeyValueList* prev;
        int replaced = 0;
        while(list != NULL) {
            // replace existing value
            if(strcmp(key, list->keyValue.key) == 0) {
                // maybe not a good idea?
                object_free(list->keyValue.value);
                list->keyValue.value = value;
                replaced = 1;
                break;
            }
            prev = list;
            list = list->next;
        }
        // append to list
        if(!replaced) {
            struct KeyValueList* node = malloc(sizeof(struct KeyValueList));
            node->next = NULL;
            node->keyValue.key = key;
            node->keyValue.value = value;
            prev->next = node;
        }
    }
    map->load ++;
}

struct Object* hashmap_get(struct HashMap* map, char* key) {
    int hash = hashmap_string_hash(key) % map->size;
    if(map->pairs[hash] == NULL) {
        return NULL;
    } else {
        struct KeyValueList* list = map->pairs[hash];
        while(list != NULL) {
            if(strcmp(key, list->keyValue.key) == 0) {
                return list->keyValue.value;
            }
            list = list->next;
        }
        return NULL;
    }
}

void hashmap_free_map(struct HashMap* map) {
    ;
}
