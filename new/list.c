#include <stdlib.h>
#include <stdio.h>

#include "object.h"

List *list_init() {
    List *list = malloc(sizeof(List));
    list->obj = NULL;
    list->next = NULL;
    return list;
}

void list_free(List *list) {
    while(list != NULL) {
        List *prev = list;
        list = list->next;
        free(prev);
    }
}

int list_length(List *list) {
    int length = 0;
    while(list != NULL) {
        if(list->obj)
            length ++;
        list = list->next;
    }
    return length;
}

List *list_append(List *list, Object *obj) {
    while(list->next != NULL) {
        list = list->next;
    }
    
    if(list->obj == NULL) {
        list->obj = obj;
        return list;
    } else {
        List *next = list_init();
        next->obj = obj;
        list->next = next;
        return next;
    }
}

List *list_copy(List *list) {
    List *new = list_init();
    List *new2 = new;
    while(list != NULL) {
        new2 = list_append(new2, list->obj);
        list = list->next;
    }
    return new;
}

void list_print(List *list) {
    while(list != NULL) {
        printf("%s\n", (char *)list->obj->data.ptr);
        list = list->next;
    }
}
