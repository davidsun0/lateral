#include <stdio.h>

#include "list.h"

#include "printer.h"

void print_string(struct Object* obj) {
    if(obj->type == list_type) {
        printf("(");
        struct List* node = obj->data.ptr;
        while(node != NULL) {
            print_string(&(node->obj));
            node = node->next;
            if(node != NULL) {
                printf(" ");
            }
        }
        printf(")");
    } else {
        object_print(obj);
    }
}
