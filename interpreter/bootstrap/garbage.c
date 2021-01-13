#include <stdlib.h>
#include <stdio.h>

#include "eval.h"
#include "object.h"

#include "garbage.h"

Bank *bank_init() {
    Bank *bank = malloc(sizeof(Bank));
    if(!bank) {
        fprintf(stderr, "out of memory while allocating object bank\n");
        exit(1);
    }

    bank->next = NULL;
    for(int i = 0; i < BANKSIZE; i ++) {
        bank->objs[i].type = empty;
        SET_MARK(&bank->objs[i]);
    }

    bank->is_full = 0;
    return bank;
}

void garbage_init() {
    all_objects = bank_init();
}

Object* garbage_alloc() {
    Bank *bank = all_objects;
    while(bank != NULL) {
        if(!bank->is_full) {
            for(int i = 0; i < BANKSIZE; i ++) {
                if(bank->objs[i].type == empty) {
                    return &bank->objs[i];
                }
            }
            bank->is_full = 1;
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

void garbage_run() {
    // loop over all obj in curr_envir
    HashMap *map = curr_envir->map;
    for(int i = 0; i < map->capacity; i ++) {
        obj_mark(CAR(map->buckets + i));
        obj_mark(CDR(map->buckets + i));
    }

    // free dead objects and unmark everything
    Bank *bank = all_objects;
    while(bank != NULL) {
        for(int i = 0; i < BANKSIZE; i ++) {
            if(!GET_MARK(&bank->objs[i]) && bank->objs[i].type != empty) {
                obj_release(&bank->objs[i]);
                bank->objs[i].type = empty;
                bank->objs[i].flags = 0;
                bank->is_full = 0;
            }
            UNSET_MARK(&bank->objs[i]);
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
