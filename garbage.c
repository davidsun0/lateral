#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"
#include "hash.h"
#include "env.h"

#include "garbage.h"

int object_count = 0;

struct List* all_objects;
extern struct Envir* global_env;

void gc_init() {
    all_objects = list_bare_init();
    object_count = 0;
}

void gc_insert_object(struct Object* obj) {
    list_bare_prepend(&all_objects, obj);
    object_count ++;
}

void gc_print(struct Object* obj) {
    object_print_type(obj->type);
    printf("\n @ %p\n", (void*) obj);
    object_print_string(obj);
    printf("\n");
}

void gc_run() {
    // mark
    // list_print(all_objects, 0);
    struct Envir* env = global_env;
    while(env != NULL) {
        struct HashMap* map = env->map;
        for(int i = 0; i < map->size; i ++) {
            if(map->pairs[i] != NULL) {
                struct KeyValueList* kvlist = map->pairs[i];
                while(kvlist != NULL) {
                    object_mark(kvlist->value);
                    kvlist = kvlist->next;
                }
            }
        }
        env = env->inner;
    }

    // sweep
    struct List* curr = all_objects;
    struct List* prev = all_objects;
    int stale_count = 0;
    object_count = 0;
    while(curr != NULL) {
        if(!curr->obj->marked) {
            object_free(curr->obj);
            if(prev != all_objects) {
                prev->next = curr->next;
            } else {
                all_objects = curr->next;
                prev = all_objects;
            }
            struct List* next = curr->next;
            free(curr);
            curr = next;
            stale_count ++;
        } else {
            curr->obj->marked = 0;
            object_count ++;
            prev = curr;
            curr = curr->next;
        }
    }
    // printf("%d objects collected\n", stale_count);
    // list_print(all_objects, 0);
}
