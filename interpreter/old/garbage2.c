#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "eval.h"
#include "object.h"

#include "garbage.h"

// if there are n live objects, the pointer map will have n*SLOT_FACTOR slots
#define SLOT_FACTOR 2

Bank *all_objects;
Bank *first_free;
int first_free_idx;

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

uintptr_t ptr_hash(uintptr_t slot_count, Object* ptr) {
    return (((uintptr_t)ptr >> 4) * 17) % slot_count;
}

void set_ptr(Object** map, uintptr_t slot_count, Object* key, Object* val) {
    uintptr_t idx = ptr_hash(slot_count, key);
    int loop_count = 0;
    while(map[idx * 2] != NULL) {
        idx ++;
        if(idx >= slot_count) {
            idx = 0;
            loop_count ++;
            if(loop_count >= 2) {
                fprintf(stderr, "could not find empty slot in garbage map\n");
                return;
            }
        }
    }
    map[idx * 2] = key;
    map[idx * 2 + 1] = val;
}

Object *get_ptr(Object** map, uintptr_t slot_count, Object* ptr) {
    uintptr_t idx = ptr_hash(slot_count, ptr);
    int loop_count = 0;
    while(map[idx * 2] != ptr) {
        idx ++;
        if(idx >= slot_count) {
            idx = 0;
            loop_count ++;
            if(loop_count >= 2) {
                fprintf(stderr, "could not find pointer in garbage map\n");
                return NULL;
            }
        }
    }
    return map[idx * 2 + 1];
}

void garbage_init() {
    all_objects = bank_init();

    first_free = all_objects;
    first_free_idx = 0;
}

Object* garbage_alloc() {
    /*
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
    */
    Object* obj = first_free->objs + first_free_idx;
    first_free_idx ++;
    if(first_free_idx >= BANKSIZE) {
        first_free_idx = 0;
        if(first_free->next == NULL) {
            first_free->next = bank_init();
        }
        first_free = first_free->next;
    }
    return obj;
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
    int obj_count = 0;
    while(bank != NULL) {
        for(int i = 0; i < BANKSIZE; i ++) {
            if(!GET_MARK(&bank->objs[i]) && bank->objs[i].type != empty) {
                obj_release(&bank->objs[i]);
                bank->objs[i].type = empty;
                bank->objs[i].flags = 0;
                bank->is_full = 0;
            } else if(bank->objs[i].type != empty) {
                obj_count ++;
            }
            UNSET_MARK(&bank->objs[i]);
        }
        bank = bank->next;
    }

    // hashmap with even indicies being keys w/ forward probing resolution
    Object** ptr_map = malloc(sizeof(Object *) * obj_count * 2 * SLOT_FACTOR);
    memset(ptr_map, 0, sizeof(Object *) * obj_count * 2 * SLOT_FACTOR);

    bank = all_objects;
    first_free = all_objects;
    first_free_idx = 0;
    while(bank != NULL) {
        for(int i = 0; i < BANKSIZE; i ++) {
            if(bank->objs[i].type != empty) {
                // add currrent_pointer, new_pointer to hashmap
                set_ptr(ptr_map,
                        obj_count * SLOT_FACTOR,
                        &bank->objs[i],
                        &first_free->objs[first_free_idx]);

                // copy to location at new_pointer
                memcpy(&first_free->objs[first_free_idx],
                        &bank->objs[i], sizeof(Object));

                // clear object at current position
                if(bank != first_free && i != first_free_idx)
                    bank->objs[i].type = empty;

                if(&first_free->objs[first_free_idx] != &bank->objs[i]) {
                    printf("moved %p to %p\n", &first_free->objs[first_free_idx],
                            &bank->objs[i]);
                    obj_debug_basic(&first_free->objs[first_free_idx]);
                    obj_debug_basic(&bank->objs[i]);
                }

                // increment first_free pointer
                first_free_idx ++;
                if(first_free_idx >= BANKSIZE) {
                    first_free = first_free->next;
                    first_free_idx = 0;
                }
            }
        }
        bank = bank->next;
    }

    // update all pointers
    bank = all_objects;
    while(bank != NULL) {
        for(int i = 0; i < BANKSIZE; i ++) {
            if(bank->objs[i].type == listt) {
                bank->objs[i].data.cell.car = get_ptr(ptr_map,
                        obj_count * SLOT_FACTOR,
                        bank->objs[i].data.cell.car);
                bank->objs[i].data.cell.cdr = get_ptr(ptr_map,
                        obj_count * SLOT_FACTOR,
                        bank->objs[i].data.cell.cdr);
                printf("%p %p\n", (void *)bank->objs[i].data.cell.car,
                        (void *)bank->objs[i].data.cell.cdr);
            } else if(bank->objs[i].type == hashmapt) {
                printf("there are maps\n");
                /*
                HashMap* map = bank->objs[i].data.hashmap;
                for(int i = 0; i < map->capacity; i ++) {
                    Object *bucket = map->buckets + i;
                    if(CAR(bucket) != nil_obj) {
                        while(bucket != nil_obj) {
                            CAR(bucket) = get_ptr(ptr_map,
                                    obj_count * SLOT_FACTOR,
                                    CAR(bucket));
                            CDR(bucket) = get_ptr(ptr_map,
                                    obj_count * SLOT_FACTOR,
                                    CDR(bucket));
                            bucket = CDR(bucket);
                        }
                    }
                }
                */
            }
        }
        bank = bank->next;
    }

    printf("nil check: %p %p %p\n", nil_obj, CAR(nil_obj), CDR(nil_obj));
    printf("[%p %d]\n", first_free, first_free_idx);
    // obj_debug(&first_free->objs[first_free_idx]);
    // obj_debug(&first_free->objs[first_free_idx - 1]);
    printf("===\n");
    bank = all_objects;
    while(bank != NULL) {
        printf("bank %p\n", bank);
        for(int i = 0; i < BANKSIZE; i ++) {
            if(bank->objs[i].type) {
                printf("%d %s\n", i, type_to_str(bank->objs[i].type));
                obj_debug_basic(&bank->objs[i]);
            }
        }
        bank = bank->next;
    }

    free(ptr_map);
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
