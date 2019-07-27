#include <stdlib.h>
#include <stdio.h>

#include "hash.h"
#include "eval.h"
#include "object.h"

#include "garbage.h"

Bank *bank_init() {
    Bank *bank = malloc(sizeof(Bank));
    if(bank == NULL) {
        printf("fatal: failed to allocate object bank\n");
        exit(1);
    }

    bank->next = NULL;
    for(int i = 0; i < BANKSIZE; i ++) {
        bank->objs[i].type = empty;
        bank->objs[i].marked = 0;
    }
    return bank;
}

void garbage_init() {
    // all_objects = list_init();
    all_objects = bank_init();
}

Object* garbage_alloc() {
    Bank *bank = all_objects;
    while(bank != NULL) {
        for(int i = 0; i < BANKSIZE; i ++) {
            if(bank->objs[i].type == empty) {
                return &bank->objs[i];
            }
        }

        if(bank->next == NULL) {
            bank->next = bank_init();
        }
        bank = bank->next;
    }
    printf("fatal: failed to allocate object\n");
    exit(1);
    return NULL;
}

/*
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
*/

void garbage_run() {
    // loop over all obj in curr_envir
    HashMap *map = curr_envir->map;
    for(int i = 0; i < map->capacity; i ++) {
        /*
        List *list = map->buckets + i;
        if(list->obj != NULL) {
            while(list != NULL) {
                obj_mark(list->obj);
                list = list->next;
            }
        }
        */
        obj_mark(CAR(map->buckets + i));
        obj_mark(CDR(map->buckets + i));
    }

    // List *objs = all_objects;
    // List *prev = all_objects;
    
    /*
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
    */

    Bank *bank = all_objects;
    while(bank != NULL) {
        for(int i = 0; i < BANKSIZE; i ++) {
            if(!bank->objs[i].marked && bank->objs[i].type != empty) {
                // obj_debug(&bank->objs[i]);
                obj_release(&bank->objs[i]);
                bank->objs[i].type = empty;
            }
            bank->objs[i].marked = 0;
        }
        bank = bank->next;
    }
}

void garbage_shutdown() {
    Bank *bank = all_objects;
    int alive = 0;
    int dead = 0;
    int banks = 0;
    while(bank != NULL) {
        banks ++;
        for(int i = 0; i < BANKSIZE; i ++) {
            if(bank->objs[i].type == empty) {
                dead ++;
            } else {
                obj_release(&bank->objs[i]);
                alive ++;
            }
        }
        Bank *prev = bank;
        bank = bank->next;
        free(prev);
    }
    printf("== gc stats ==\n");
    printf("banks in use:%d\nlive objs: %d\ndead objs: %d\n", banks, alive, dead);
}
