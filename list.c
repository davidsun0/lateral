#include <stdlib.h>
#include <stdio.h>

#include "list.h"

struct List* list_append(struct List* list, void* data) {
    while(list->next != NULL) {
        list = list->next;
    }

    if(list->data == NULL) {
        list->data = data;
        return list;
    } else {
        struct List* node = malloc(sizeof(struct List));
        node->data = data;
        node->next = NULL;
        list->next = node;
        return node;
    }
}

void list_print(struct List* list) {
    while(list != NULL) {
        printf("node adr: %p\n", (void*) list);
        printf("data adr: %p\n", list->data);
        if(list->data != NULL) {
            printf("data: %s\n\n", (char*) list->data);
        }
        list = list->next;
    }
}

void list_free(struct List* list) {
    struct List* next;
    while(list != NULL) {
        next = list->next;
        if(list->data != NULL) {
            free(list->data);
        }
        free(list);
        list = next;
    }
}
