#include <stdlib.h>
#include <stdio.h>

#include "list.h"
#include "hash.h"
#include "eval.h"
#include "object.h"

#include "garbage.h"

void garbage_init() {
    all_objects = list_init();
}

void garbage_insert(Object *obj) {
    obj->marked = 0;
    if(all_objects->obj != NULL) {
        List *head = list_init();
        head->next = all_objects;
        head->obj = obj;
        all_objects = head;
    } else {
        all_objects->obj = obj;
    }
}

void garbage_run() {
    // loop over all obj in curr_envir
    HashMap *map = curr_envir->map;
    for(int i = 0; i < map->capacity; i ++) {
        List *list = map->buckets + i;
        if(list->obj != NULL) {
            while(list != NULL) {
                obj_mark(list->obj);
                list = list->next;
            }
        }
    }

    List *objs = all_objects;
    List *prev = all_objects;
    
    while(objs != NULL) {
        if(!objs->obj->marked) {
            List *old = objs;
            if(old != all_objects) {
                prev->next = old->next;
            } else {
                all_objects = old->next;
                prev = all_objects;
            }
            objs = objs->next;
            obj_free(old->obj);
            free(old);
        } else {
            objs->obj->marked = 0;
            prev = objs;
            objs = objs->next;
        }
    }
}
