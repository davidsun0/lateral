#ifndef LA_LIST_H
#define LA_LIST_H

#include "object.h"

List *list_init();
void list_free(List *);

int list_length(List *list);
List *list_append(List *, Object *);
void list_print(List *);

#endif
