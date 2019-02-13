#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"
#include "hash.h"
#include "env.h"

#include "garbage.h"

#define MAX_OBJ_COUNT 256

int object_count = 0;
int max_object_count = MAX_OBJ_COUNT;

struct List* all_objects;
extern struct Envir* global_env;
extern struct Envir* user_env;

struct Object** stack_base;

void gc_init(void* ptr) {
    all_objects = list_bare_init();
    object_count = 0;
    stack_base = ptr;
}

void gc_insert_object(struct Object* obj) {
    list_bare_prepend(&all_objects, obj);
    object_count ++;
    if(object_count >= max_object_count) {
        gc_run();
        // gc_scan_stack();
        // max_object_count *= 2;
    }
}

void gc_print(struct Object* obj) {
    object_print_type(obj->type);
    printf(" @ %p\n", (void*) obj);
    object_print_string(obj);
    printf("\n");
}

void gc_heap_bounds(struct Object** left, struct Object** right) {
    struct Object* max = all_objects->obj;
    struct Object* min = all_objects->obj;
    struct List* list = all_objects->next;
    while(list != NULL) {
        if(max < list->obj)
            max = list->obj;
        else if(min > list->obj)
            min = list->obj;
        list = list->next;
    }
    *left = min;
    *right = max;
}

void gc_scan_stack() {
    struct Object* heap_min = NULL;
    struct Object* heap_max = NULL;
    struct Object** stack_top = &heap_min;

    gc_heap_bounds(&heap_min, &heap_max);
    // printf("hbase: %p\nhtop: %p\n", (void*) heap_min, (void*) heap_max);
    // printf("sbase: %p\nstop: %p\n", (void*) stack_base, (void*) stack_top);
    // for(void* ptr = stack_top; ptr < stack_base; ptr = ((char*) ptr) + sizeof(void*)) {
    for(struct Object** ptr = stack_top;
            ptr < stack_base;
            ptr ++) {

        // struct Object* stack_var = *ptr;
        struct Object* obj = *ptr;
        if(heap_min < obj && obj < heap_max) {
            // printf("%p\n", (void*) *ptr);
            struct List* list = all_objects;
            while(list != NULL) {
                if(obj == list->obj) {
                    // printf("\n");
                    // gc_print(list->obj);
                    // printf("\n");
                    /*
                    printf("mark %p ", (void*) list->obj);
                    printf("%d ", list->obj->marked);
                    printf("%d\n", list->obj->marked);
                    */
                    object_mark(list->obj);
                    break;
                }
                list = list->next;
            }
        }
    }
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

    gc_scan_stack();
    /*
    struct List* working = working_objects->data.ptr;
    while(working != NULL) {
        object_mark(working->obj);
        working = working->next;
    }
    */

    // sweep
    struct List* curr = all_objects;
    struct List* prev = all_objects;
    int stale_count = 0;
    object_count = 0;
    while(curr != NULL) {
        if(!curr->obj->marked) {
            // printf("%d\n", curr->obj->marked);
            // printf("free %p\n", (void*) curr->obj);
            // object_print_string(curr->obj);
            // printf("\n");
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
            // curr->obj->marked = 0;
            object_count ++;
            prev = curr;
            curr = curr->next;
        }
    }

    curr = all_objects;
    while(curr != NULL) {
        curr->obj->marked = 0;
        curr = curr->next;
    }

    printf("gc: %d stale/%d live objects collected\n", stale_count, object_count);
    // list_print(all_objects, 0);
    if(object_count >= max_object_count) {
        max_object_count *= 2;
    }
}

void gc_delete_everything_yes_im_sure() {
    hashmap_free_map(global_env->map);
    hashmap_free_map(user_env->map);

    struct List* node = all_objects;
    while(node != NULL) {
        object_free(node->obj);
        struct List* next = node->next;
        free(node);
        node = next;
    }
}
