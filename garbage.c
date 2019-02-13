#include <stdlib.h>
#include <stdio.h>

#include "object.h"
#include "list.h"
#include "hash.h"
#include "env.h"

#include "garbage.h"

#define MAX_OBJ_COUNT 256

extern struct Envir* global_env;
extern struct Envir* user_env;

static int object_count = 0;
static int max_object_count = MAX_OBJ_COUNT;

static struct List* all_objects;

// pointer to a local variable in main()
// used for finding pointers on the stack
static struct Object** stack_base;

void gc_init(void* ptr) {
    all_objects = list_bare_init();
    object_count = 0;
    stack_base = ptr;
}

void* gc_malloc(size_t size) {
    void* ptr = malloc(size);
    if(ptr == NULL) {
        printf("warning: malloc failed\n");
        gc_run();
        ptr = malloc(size);
        if(ptr == NULL) {
            printf("fatal: out of memory\n");
            exit(1);
        }
    }
    return ptr;
}

void gc_insert_object(struct Object* obj) {
    list_bare_prepend(&all_objects, obj);
    object_count ++;
    if(object_count >= max_object_count) {
        gc_run();
    }
}

void gc_print(struct Object* obj) {
    object_print_type(obj->type);
    printf(" @ %p\n", (void*) obj);
    object_print_string(obj);
    printf("\n");
}

static void gc_heap_bounds(struct Object** left, struct Object** right) {
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

static void gc_scan_stack() {
    struct Object* heap_min = NULL;
    struct Object* heap_max = NULL;
    struct Object** stack_top = &heap_min;

    // knowing heap bounds speeds up object search
    gc_heap_bounds(&heap_min, &heap_max);

    // TODO: add code for stacks that expand the other way
    // iterate through the memory of the entire stack
    for(struct Object** ptr = stack_top; ptr < stack_base; ptr ++) {
        struct Object* obj = *ptr;
        if(heap_min < obj && obj < heap_max) {
            // mark if data is a pointer to an object
            struct List* list = all_objects;
            while(list != NULL) {
                if(obj == list->obj) {
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
    // mark objects in the environment
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

    // mark objects on the stack
    gc_scan_stack();

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
            object_count ++;
            prev = curr;
            curr = curr->next;
        }
    }

    // unmark all objects
    curr = all_objects;
    while(curr != NULL) {
        curr->obj->marked = 0;
        curr = curr->next;
    }

    printf("gc: %d stale/%d live objects collected\n", stale_count, object_count);
    if(object_count >= max_object_count) {
        max_object_count *= 2;
    }
}
